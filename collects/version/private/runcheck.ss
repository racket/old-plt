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
      (define dialog-title "PLT version status")

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

      (define timeout-value 60)

      (define the-port #f)

      (define (timer-proc)
	(let loop ([n 0])
	  (if (> n timeout-value)
	      (begin
		(show-ok "Network timeout" 
			 "Can't connect to PLT version server")
		(when the-port
                      ; will force exception on pending read
		      (close-input-port the-port)))
	      (begin
		(sleep 1)
		(loop (add1 n))))))

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
      (define (bad-collects)
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

      (define (extract-bad-collection-names b-c syms)
	(map car 
	     (filter (lambda (c)
		       (member (cadr c) syms)
		       b-c))))

      (define (cvi-triples)
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

      (define (go)

	(let ([b-c (bad-collects)])

	  (if (not (null? b-c))

	      (let ([bad-info 
		     (extract-bad-collection-names b-c (list no-info-sym))]
		    [no-version 
		     (extract-bad-collection-names b-c (list no-release-sym no-iter-sym))])
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
				no-version))))))

	      ; no bad collections

	      (when (or (not (null? args-vis))
			(eq? 'yes
			     (get-yes-no "PLT version check" 
					 check-question)))
		    (set! the-port 
			  (with-handlers 
			   ((void 
			     (lambda _ 
			       (show-error-ok
				"Network failure" 
				"Can't connect to PLT version server")
			       (raise 'network-error))))
			   (get-pure-port (string->url 
					   (make-url-string
					    (cvi-triples))))))
	      
		    (let* ([timeout-thread (thread timer-proc)]
			   [responses 
			    (let loop ()
			      (let ([r (read the-port)])
				(if (eof-object? r)
				    (begin 
				      (kill-thread timeout-thread)
				      '())
				    (cons r (loop)))))]
			   [curr-version (version)]
			   [needs-update #f])
		  
		      (close-input-port the-port)

                ; responses are a list of lists of symbol/string pairs: 
                ;  (((package name)	
	        ;    (installed-version v)
                ;    (installed-iteration v)
	        ;    (latest-version v)
	        ;    (latest-iteration v)
                ;    (verdict s))
                ;    ... )

	        ; first handle binary info, which is always first in responses

		      (let*-values
		       ([(_ binary-version binary-iteration 
			    latest-binary-version latest-binary-iteration 
			    binary-verdict)
			 (apply values (map cadr (car responses)))])
		       
		       (if (eq? binary-verdict 'update)
			   
                     ; inform user of new binary 

			   (show-ok 
			    dialog-title
			    (string-append
			     "Installed binaries for DrScheme (or MzScheme) "
			     "are not up-to-date"
			     nl nl
			     (format 
			      "Installed binary version: ~a (iteration ~a)"
			      binary-version binary-iteration)
			     nl nl
			     (format 
			      "Latest released version: ~a (iteration ~a)"
			      latest-binary-version latest-binary-iteration)
			     nl nl
			     "Updates are available at "
			     download-url-string))
		
                      ; else offer info for installed packages

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
				    (cdr responses))]
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
			      dialog-title
			      (if needs-update
				  (string-append
				   folded-string
				   (string #\newline #\newline)
				   "Updates are available at "
				   download-url-string)
				  folded-string))))))))))

      ; exceptions are used to report errors elsewhere
      ; just ignore here

      (with-handlers
       ((void void))
       (go)))))


