(define mred:debug:turned-on (box (list 'load 'startup 'html 'dv)))

(define mred:debug@
  (let* ([debug-env (getenv "MREDDEBUG")]
	 [turned-on mred:debug:turned-on])
    (unit (import)
      (export (dprintf printf) exit? on? turn-on turn-off)

      (define on? (and debug-env (read (open-input-string debug-env))))

      (define turn-on (lambda (s)
			(set-box! turned-on (cons s (unbox turned-on)))))
      (when on?
	(if (list? on?)
	    (for-each turn-on on?)
	    (turn-on on?))
	(print-struct #t)
	(printf "turned on: ~a~n" (unbox turned-on)))

      (define turn-off
	(lambda (s) 
	  (set-box! turned-on
		    (let loop ([l (unbox turned-on)])
		      (cond
		       [(null? l) null]
		       [else (if (eq? (car l) s)
				 (loop (cdr l))
				 (cons (car l)
				       (loop (cdr l))))])))))

      (define param (current-parameterization))

      (define dprintf (if on? 
			  (lambda (tag . args)
			    (with-parameterization param
			      (lambda ()
				(when (member tag (unbox turned-on))
				  (apply printf args)
				  (newline)))))
			  (lambda args (void))))

      (define exit? #t))))

(invoke-open-unit mred:debug@ mred:debug)

(define-macro mred:dv
  (lambda args
    (when mred:debug:on?
      (let ([string
	     (let loop ([string ""] [args args])
	       (cond
		 [(null? args) string]
		 [else 
		  (unless (symbol? (car args))
		    (error 'mred:dv 
			   "only accepts symbols as arguments"))
		  (loop 
		   (string-append string 
				  (symbol->string (car args))
				  ": ~s ")
		   (cdr args))]))])
	`(mred:debug:printf 'dv ,string ,@args)))))

(when mred:debug:on?
  (letrec* ([old-handler (current-load)]
	    [offset-string "  "]
	    [indent-string ""]
	    [link?
	     (lambda (x)
	       (let ([len (string-length x)])
		 (or (and (> len 4)
			  (string=? (substring x (- len 4) len) "link"))
		     (and (> len 6)
			  (string=? (substring x (- len 6) len) "macros")))))])
    (current-load (lambda (f)
		    (let* ([file (if (relative-path? f)
				     (build-path (current-directory) f)
				     f)])
		      (mred:debug:printf 'load "~aLoading ~a..." indent-string file)
		      (let* ([answer
			      (dynamic-wind (lambda ()
				      (set! indent-string (string-append offset-string indent-string)))
				    (lambda () (old-handler file))
				    (lambda ()
				      (set! indent-string
					    (substring indent-string
						       0
						       (max (- (string-length indent-string)
							       (string-length offset-string))
							    0)))))]
			     [len (string-length file)]
			     [basename (substring file 0 (- len 3))]
			     [suffix (substring file (- len 3) len)]
			     [zo (string-append basename ".zo")]
			     [error-handler
			      (lambda (e) 
				(delete-file zo)
				((error-display-handler)
				 (string-append indent-string
						(exn-message e)))
				#f)])
			(when (and (or (eq? mred:debug:on? 'compile)
				       (eq? mred:debug:on? 'compile-and-exit))
				   (not (link? basename))
				   (string=? ".ss" suffix)
				   (or (not (file-exists? zo))
				       (<= (file-modify-seconds zo)
					   (file-modify-seconds file))))
			  (mred:debug:printf 'load "~aCompiling ~a..." indent-string file)
			  (with-handlers ((void error-handler))
			    (compile-file file zo)
			    #t))
			answer))))))

(define mred:debug:new-eval #f)
(define mred:debug:new-console (void))  
(define mred:debug:carryover-ids 
  (list 'mred:system-source-directory
	'mred:make-mred-invokable-unit
	'mred:debug:turned-on))
(define mred:debug:make-new-console
  (if mred:debug:on? 
      (lambda ()
	(when mred:debug:new-eval
	  (let ([old mred:debug:new-eval])
	    (set! mred:debug:new-eval #f)
	    (old '(mred:run-exit-callbacks))))
	(set! mred:debug:new-eval (make-eval 'wx))
	(set! mred:make-application@ mred:non-unit-make-application@)
	(load-recent "link")
	(for-each (lambda (id) 
		     (let ([value (global-defined-value id)])
		       (printf "defining ~a~n" id)
		       (mred:debug:new-eval `(define ,id ,value))))
		  mred:debug:carryover-ids)
	(set! mred:debug:new-console 
	      (mred:debug:new-eval 
	       `(begin
		  (current-library-path ,(current-library-path))
		  (require-library "core.ss")
		  (require-library "pconvert.ss")
		  (load (build-path mred:system-source-directory "sig.ss"))
		  (invoke-open-unit ,(mred:make-invokable-unit) #f))))
	mred:debug:new-eval)
      (lambda () (void))))