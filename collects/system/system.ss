;; dont open a spash screen if the splash image is #f

;(load-relative "loader.ss")

(error-print-width 250)

(when (getenv "MREDCOMPILE")
  (load-relative "compsys.ss"))

(require-library "refer.ss")

(load-relative "debug.ss")
(load-relative "invsig.ss")
(load-relative "invoke.ss")
(load-relative "splash.ss")

(mred:debug:printf 'startup "current-library-collection-paths: ~s"
		   (current-library-collection-paths))

(define mred:startup
  (lambda args
    (let* ([orig-width 100]
	   [orig-height 100]
	   [frame (make-object wx:frame% null "mred:startup default" 
			       orig-width orig-height)]
	   [panel (make-object wx:panel% frame)]
	   [button (make-object wx:button% 
				panel 
				(lambda (button evt) (exit))
				"Quit")]
	   [bw (send button get-width)]
	   [bh (send button get-height)]
	   [w (box 0)]
	   [h (box 0)])
      (send button set-size 0 0 bw bh)
      (send frame get-client-size w h)
      (let ([new-w (+ bw (- orig-width (unbox w)))]
	    [new-h (+ bh (- orig-height (unbox h)))])
	(send frame set-size 0 0 new-w new-h)
	(send panel set-size 0 0 new-w new-h))
      (send frame show #t))))

;; called with the arguments on the command line
(define mred:initialize
  (lambda input-args
    (let* ([user-setup? #t]
	   [output-spidey-file #f]
	   [app-determined? #f]
	   [app-name "MrEd"]
	   [app-collection "system"]
	   [app-unit-library "nuapp.ss"]
	   [app-sig-library "sig.ss"]
	   [splash-path (with-handlers ([(lambda (x) #t)
					 (lambda (x) "mred.gif")])
			  (build-path (collection-path "icons") "mred.gif"))]
	   [splash-depth 4]
	   [splash-max 73]
	   [basic-info (lambda (sym)
			 (case sym
			   [(app-unit-library) app-unit-library]
			   [(app-sig-library) app-sig-library]
			   [(name) app-name]
			   [(splash-image-path) splash-path]
			   [(splash-depth) splash-depth]
			   [(splash-max) splash-max]
			   [else (error 'basic-info "unexpected symbol: ~a~n" sym)]))]
	   [app-determined
	    (lambda ()
	      (when app-determined?
		(error "Conflicting -A, -a, and/or -nu flags"))
	      (set! app-determined? #t))]
	   [remaining-args
	    (let loop ([args input-args])
	      (cond
	       [(null? args) null]
	       [else 
		(let* ([arg (car args)]
		       [rest (cdr args)]
		       [use-next-arg
			(lambda (f)
			  (if (null? rest)
			      (error "expected another arg after ~a" arg)
			      (begin (f (car rest))
				     (loop (cdr rest)))))]
		       [use-next-2args
			(lambda (f)
			  (if (or (null? rest)
				  (null? (cdr rest)))
			      (error "expected another 2 args after ~a" arg)
			      (begin (f (car rest) (cadr rest))
				     (loop (cddr rest)))))]
		       [use-next-3args
			(lambda (f)
			  (if (or (null? rest)
				  (null? (cdr rest))
				  (null? (cddr rest)))
			      (error "expected another three args after ~a" arg)
			      (begin (f (car rest) (cadr rest) (caddr rest))
				     (loop (cdddr rest)))))]
		       [use-next-4args
			(lambda (f)
			  (if (or (null? rest)
				  (null? (cdr rest))
				  (null? (cddr rest))
				  (null? (cdddr rest)))
			      (error "expected another four args after ~a" arg)
			      (begin (f (car rest) (cadr rest) (caddr rest) (cadddr rest))
				     (loop (cddddr rest)))))])
		  (cond
		   [(string=? "-w" arg)
		    (use-next-arg
		     (lambda (arg)
		       (set! output-spidey-file arg)))]
		   [(string=? "-A" arg)
		    (app-determined)
		    (use-next-arg
		     (lambda (collection)
		       (set! app-collection collection)
		       (set! basic-info (require-library "info.ss" collection))))]
		   [(string=? "-a" arg)
		    (app-determined)
		    (use-next-3args
		     (lambda (collection unit-lib sig-lib)
		       (set! app-collection collection)
		       (set! app-unit-library unit-lib)
		       (set! app-sig-library sig-lib)))]
		   [(string=? "-b" arg) 
		    (set! splash-path #f)
		    (loop rest)]
		   [(string=? "-p" arg)
		    (use-next-4args
		     (lambda (image title num-files depth)
		       (set! splash-path image)
		       (set! app-name title)
		       (let ([max (string->number num-files)]
			     [deep (string->number depth)])
			 (if (and (number? max)
				  (number? deep))
			     (begin
			       (set! splash-max max)
			       (set! splash-depth deep))
			     (error 'startup 
				    "-p flag expects the 3rd and 4th arguments to be numbers, got ~a ~a")))))]
		   [(string=? "--" arg)
		    args]
		   [(string=? "-nu" arg)
		    (app-determined)
		    (loop rest)]
		   [else (cons arg (loop rest))]))]))])
      (unless app-determined?
	(set! basic-info (require-library "info.ss" "system")))
      (when output-spidey-file
	((load-relative "spidey.ss") output-spidey-file app-collection basic-info))
      (mred:startup-application app-collection basic-info remaining-args))))
