(module runcheck mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "list.ss"))
  (require (lib "cmdline.ss"))
  (require (lib "url.ss" "net"))
  (require (lib "getinfo.ss" "setup"))

  (require "checksigs.ss")

  (provide runcheck@)

  (define runcheck@
    (unit/sig empty^

      (import defs^)

      (define argv (namespace-variable-binding 'argv))

      (define command-collects '())

      (parse-command-line 
	 progname
	 argv
	 `((multi 
	    [("-l")
	     ,(lambda (flag col)
		(set! command-collects
		      (cons col command-collects)))
	     ("represents a package" "collection")]))
	 (lambda (x) void)
	 '())

      (define download-url-string "http://download.plt-scheme.org/")

      (define check-question 
	"Check versions of all PLT software over the Internet?")

      (define (mk-starred-string s a)
	(if a
	    (string-append s "*" a)
	    s))

      (define (fold-starred-string ss)
	(foldr mk-starred-string #f ss))

      (define (make-url-string vcs)
	(format 
	 (string-append
	 "http://download.plt-scheme.org/cgi-bin/check-version?"
	 "packages=~a&"
	 "versions=~a")
	 (fold-starred-string (map car vcs))
	 (fold-starred-string (map cadr vcs))))
      
      (define newer-version? string>=?)

      (define timeout 60)
      
      (define (timer-proc)
	(let loop ([n 0])
	  (when (> n timeout)
		(show-ok "Network timeout" 
			 "Can't connect to PLT version server")
		(exit))
	  (sleep 1)
	  (loop (add1 n))))

      (define (get-collect-version collect)
	(with-handlers 
	 ([void (lambda _ #f)])
	 ((get-info (list collect)) 'release-version 
	  (lambda _ #f))))

      (define command-versions
	(map
	 (lambda (c)
	   (list c (get-collect-version c)))
	 command-collects))

      (define bad-collects
	(map car
	     (filter 
	      (lambda (c)
		(not (cadr c)))
	      command-versions)))
	
      (unless (null? bad-collects)
	      (show-ok "Invalid package collections"
		       (string-append
			"The specified collections are not installed:"
			(string #\newline)
			(foldr
			 (lambda (s a)
			   (if a
			       (string-append 
				s
				", "
				a)
			       s))
			 #f
			 bad-collects)))
	      (exit 1))

      (define version-collects
	(if (not (null? command-versions))

	    ; user-specified
	    command-versions

	    ; find all collections
	    (let ([collects-dirs (current-library-collection-paths)])
	      (let outer-loop ([collects-dirs collects-dirs])
		(if (null? collects-dirs)
		    '()
		    (let* ([curr-collects-dir (car collects-dirs)]
			   [dirs (filter 
				  (lambda (d)
				    (directory-exists?
				     (build-path 
				      curr-collects-dir d)))
				  (directory-list curr-collects-dir))])
		      (let inner-loop ([dirs dirs])
			(if (null? dirs)
			    (outer-loop (cdr collects-dirs))
			    (let* ([curr-dir (car dirs)]
				   [dir-version
				    (get-collect-version curr-dir)])
			      (if dir-version
				  (cons `(,curr-dir ,dir-version)
					(inner-loop (cdr dirs)))
				  (inner-loop (cdr dirs))))))))))))

      (when (or (not (null? command-collects))
		(eq? 'yes
		     (get-yes-no "PLT version check" 
				 check-question)))
	    (let* ([port (with-handlers 
			  ((void 
			    (lambda _ 
			      (show-error-ok
			       "Network failure" 
			       "Can't connect to PLT version server")
			      (exit))))
			  (get-pure-port (string->url 
					  (make-url-string
					   version-collects))))]
		   [timeout-thread (thread timer-proc)]
		   [responses 
		    (let loop ()
		      (let ([r (read port)])
			(if (eof-object? r)
			    '()
			    (cons r (loop)))))]
		   [_ (kill-thread timeout-thread)]
		   [curr-version (version)]
		   [needs-update #f])
		  
	      (close-input-port port)

              ; responses are a list of lists of symbol/string pairs: 
              ;  (((package name)	
	      ;    (installed-version v)
	      ;    (latest-version v)
              ;    (verdict s))
              ;    ... )

	      ; single out DrScheme, MzScheme records, if they exist
	      (let* ([get-rec
		      (lambda (pkg) 
			(let* ([raw 
				(filter
				 (lambda (r)
				   (string=? (cadar r)
					     pkg))
				 responses)])
			  (if raw (car raw) #f)))]
		     [get-rec-field
		      (lambda (pkg f)
			(let ([record (get-rec pkg)])
			  (if record (f record) #f)))]
		     [old?
		      (lambda (pkg)
			(let ([verdict 
			       (get-rec-field pkg (lambda (x) 
						    (cadar (cdddr x))))])
			  (and verdict
			       (not (eq? 'up-to-date verdict)))))])
		
		(when (old? "DrScheme")	
		      (show-ok "PLT version status"
			       (string-append 
				(format "DrScheme v.~a needs updating to v.~a"
					; gets installed version
					(get-rec-field "DrScheme" (lambda (x)
								  (cadar (cdr x))))
					; gets latest version
					(get-rec-field "DrScheme" 
						       (lambda (x)
							 (cadar (cddr x)))))
				(string #\newline)
				"Upgrade it before updating any other packages"
				(string #\newline #\newline)
				"The latest version is available at " 
				download-url-string))
		      (exit)))

	      (let* ([all-strings
		      (map 
		       (lambda (r)
			 (let* ([data (map cadr r)]
				[package (car data)]
				[installed-version (cadr data)]
				[latest-version (caddr data)]
				[verdict (cadddr data)])
			   (cond
			    [(eq? verdict 'up-to-date)
			     (format "~a v.~a is up to date"
				     package installed-version)]
			    [(eq? verdict 'update)
			     (begin
				   (set! needs-update #t)
				   (format "~a v.~a needs updating to v.~a"
					   package installed-version latest-version))]
			    [else ""])))
		       responses)]
		     [folded-string
		      (foldr 
		       (lambda (s a)
			 (if a 
			     (if (string=? s "")
				 a
				 (string-append s (string #\newline) a))
			     s))
		       #f
		       all-strings)])

		(show-ok 
		 "PLT version status"
		 (if needs-update
		     (string-append
		      folded-string
		      (string #\newline #\newline)
		      "Updates are available at "
		      download-url-string)
		     folded-string))))))))



		    

	      
