(module runcheck mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "list.ss"))
  (require (lib "url.ss" "net"))
  (require (lib "getinfo.ss" "setup"))

  (require "checksigs.ss")

  (provide runcheck@)

  (define runcheck@
    (unit/sig empty^

      (import defs^ args^)

      (define download-url-string "http://download.plt-scheme.org/")

      (define check-question 
	"Check versions of all PLT software over the Internet?")

      (define star "*")

      (define rv-sym 'release-version)
      (define no-info-sym 'no-info-file)
      (define no-release-sym 'no-release-info)
      (define iter-sym 'release-iteration)
      (define no-iter-sym 'no-iteration-info)

      (define nl (string #\newline))
      (define (empty-string? s) (string=? s ""))
	
      (define (make-url-string vcs)
	(string-append
	 (apply string-append
		"http://download.plt-scheme.org/cgi-bin/check-version?"
		(map 
		 (lambda (cv)
		   (string-append
		    "package="
		    (car cv)
		    star
		    (cadr cv)
		    star
		    (caddr cv)
		    "&"))
		 vcs))
	 "binary-version="
	 (version)))

      (define newer-version? string>=?)

      (define timeout-value 60)
      
      (define (timer-proc)
	(let loop ([n 0])
	  (when (> n timeout-value)
		(show-ok "Network timeout" 
			 "Can't connect to PLT version server")
		(raise 'timeout))
	  (sleep 1)
	  (loop (add1 n))))

      (define (get-collect-version-info collect)
	(let ([info-proc 
	       (get-info (list collect))])
	  (if (not info-proc)
	      (list no-info-sym no-info-sym)
	      (list (info-proc rv-sym
			       (lambda _ no-release-sym))
		    (info-proc iter-sym
			       (lambda _ no-iter-sym))))))

      (define args-vis
	(map
	 (lambda (c)
	   (cons c 
		 (get-collect-version-info c)))
	 collections))

      ; list of collections with no release-version 
      ;  or version-iteration
      (define bad-collects
	(map car
	     (filter 
	      (lambda (c)
		(ormap (lambda (x)
			 (not (string? x)))
		       (cdr c)))
	      args-vis)))

      (define (comma-proc s a)
	(lambda (s a)
	  (if a
	      (string-append 
	       s
	       ", "
	       a)
	      s)))

      (define (extract-bad-collection-names sym)
	(map car 
	     (filter (lambda (c)
		       (eq? (cadr c) sym)
		       bad-collects))))

      (unless (null? bad-collects)
	      (let ([bad-info 
		     (extract-bad-collection-names no-info-sym)]
		    [no-version 
		     (extract-bad-collection-names no-release-sym)])
		(show-ok "Invalid package collections"
			 (string-append 
			  (if (null? bad-info)
			      ""
			      (string-append
			       "These collections are not installed:"
			       nl
			       (foldr
				comma-proc
				#f
				bad-info)))
			  (if (null? no-version)
			      ""
			      (string-append
			       (if (null? bad-info) ; add newline
				   ""
				   nl)
			       "These collections have missing or incomplete version information:"
			       nl
			       (foldr
				comma-proc
				#f
				no-version)))))
		(raise 'bad-collections)))

      (define cvi-triples
	(if (not (null? args-vis))

	    ; user-specified
	    args-vis

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
				   [dir-version-and-iteration
				    (get-collect-version-info curr-dir)])
			      (if (andmap string? dir-version-and-iteration) ; not a symbol indicating an error
				  (cons (cons curr-dir dir-version-and-iteration)
					(inner-loop (cdr dirs)))
				  (inner-loop (cdr dirs))))))))))))

      (printf "cv-triples: ~a~n" cvi-triples)
      (printf "url: ~a~n" (make-url-string cvi-triples))

      (when (or (not (null? cvi-triples))
		(eq? 'yes
		     (get-yes-no "PLT version check" 
				 check-question)))
	    (let* ([port (with-handlers 
			  ((void 
			    (lambda _ 
			      (show-error-ok
			       "Network failure" 
			       "Can't connect to PLT version server")
			      (raise 'network-error))))<
			  (get-pure-port (string->url 
					  (make-url-string
					   cvi-triples))))]
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

	      (printf "responses: ~a~n" responses)

              ; responses are a list of lists of symbol/string pairs: 
              ;  (((package name)	
	      ;    (installed-version v)
              ;    (installed-iteration v)
	      ;    (latest-version v)
	      ;    (latest-iteration v)
              ;    (verdict s))
              ;    ... )

	      (let* ([all-strings
		      (map 
		       (lambda (r)
			 (let*-values
			  ([(data) (map cadr r)]
			   [(package installed-version installed-iteration 
				     latest-version latest-iteration verdict)
			    (apply values data)])
			  (cond
			   [(eq? verdict 'up-to-date)
			    (format "~a v.~a (iteration ~a) is up to date"
				    package installed-version installed-iteration)]
			   [(eq? verdict 'update)
			    (begin
			      (set! needs-update #t)
			      (format "~a v.~a (iteration ~a) needs updating to v.~a (iteration ~a)"
				      package 
				      installed-version installed-iteration 
				      latest-version latest-iteration))]
			   [else ""])))
		       responses)]
		     [folded-string
		      (foldr 
		       (lambda (s a)
			 (if a 
			     (if (empty-string? s)
				 a
				 (string-append s nl a))
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
		    

	      
