
(module cmdline mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "contract.ss")
	   (lib "mred.ss" "mred")
	   (lib "cmdline.ss")
	   (lib "mrpict.ss" "texpict")
	   (lib "utils.ss" "texpict")
	   (lib "math.ss")
	   "sig.ss")

  (provide cmdline@)

  (define-syntax (define-at-end stx)
    (syntax-case stx ()
      [(_ sig body ...)
       (with-syntax ([(id ...) (let ([expr
				      (local-expand 
				       #'(signature->symbols sig)
				       'expression
				       null)])
				 (map (lambda (id)
					(datum->syntax-object
					 #'sig
					 (syntax-e id)))
				      (vector->list
				       (syntax-e (cadr (syntax->list expr))))))])
	 #'(define-values (id ...)
	     (let ()
	       body ...
	       (values id ...))))]))

  (define cmdline@
    (unit/sig cmdline^
      (import)
      
      (define-at-end cmdline^
	(define-values (screen-w screen-h) (values 1024 768))
	(define base-font-size 32)

	(define-values (actual-screen-w actual-screen-h) (get-display-size #t))
	(define-values (use-screen-w use-screen-h) (values actual-screen-w actual-screen-h))

	(define condense? #f)
	(define printing? #f)
	(define commentary? #f)
	(define show-gauge? #f)
	(define keep-titlebar? #f)
	(define show-page-numbers? #t)
	(define quad-view? #f)
	(define print-slide-seconds? #f)
	(define use-offscreen? #t)
	(define use-transitions? use-offscreen?)
	(define talk-duration-minutes #f)
	(define trust-me? #f)
	(define no-squash? #t)
	(define two-frames? #f)
	(define use-prefetch? #t)
	(define use-prefetch-in-preview? #f)
	(define print-target #f)
	
	(define init-page 0)
	
	(define file-to-load
	  (command-line
	   "slideshow"
	   (current-command-line-arguments)
	   [once-each
	    (("-d" "--preview") "show next-slide preview (useful on a non-mirroring display)" 
	     (set! two-frames? #t))
	    (("-p" "--print") "print"
	     (set! printing? #t))
	    (("-o") file "set output file for printing"
	     (set! print-target file))
	    (("-c" "--condense") "condense"
	     (set! condense? #t))
	    (("-t" "--start") page "set the starting page"
	     (let ([n (string->number page)])
	       (unless (and n 
			    (integer? n)
			    (exact? n)
			    (positive? n))
		 (error 'slideshow "argument to -t is not a positive exact integer: ~a" page))
	       (set! init-page (sub1 n))))
	    (("-q" "--quad") "show four slides at a time"
	     (set! quad-view? #t))
	    (("-n" "--no-stretch") "don't stretch the slide window to fit the screen"
	     (when (> actual-screen-w screen-w)
	       (set! actual-screen-w screen-w)
	       (set! actual-screen-h screen-h)))
	    (("-s" "--size") w h "use a <w> by <h> window"
	     (let ([nw (string->number w)]
		   [nh (string->number h)])
	       (unless (and nw (< 0 nw 10000))
		 (error 'slideshow "bad width: ~e" w))
	       (unless (and nw (< 0 nh 10000))
		 (error 'slideshow "bad height: ~e" h))
	       (set! actual-screen-w nw)
	       (set! actual-screen-h nh)))
	    (("-a" "--squash") "scale to full window, even if not 4:3 aspect"
	     (set! no-squash? #f))
	    ;; Disable --minutes, because it's not used
	    #;
	    (("-m" "--minutes") min "set talk duration in minutes"
	     (let ([n (string->number min)])
	       (unless (and n 
			    (integer? n)
			    (exact? n)
			    (positive? n))
		 (error 'slideshow "argument to -m is not a positive exact integer: ~a" min))
	       (set! talk-duration-minutes n)))
	    (("-i" "--immediate") "no transitions"
	     (set! use-transitions? #f))
	    (("--trust") "allow slide program to write files and make network connections"
	     (set! trust-me? #t))
	    (("--no-prefetch") "disable next-slide prefetch"
	     (set! use-prefetch? #f))
	    (("--preview-prefetch") "use prefetch for next-slide preview"
	     (set! use-prefetch-in-preview? #t))
	    (("--keep-titlebar") "give the slide window a title bar and resize border"
	     (set! keep-titlebar? #t))
	    (("--comment") "display commentary"
	     (set! commentary? #t))
	    (("--time") "time seconds per slide" (set! print-slide-seconds? #t))]
	   [args slide-module-file
		 (cond
		  [(null? slide-module-file) #f]
		  [(null? (cdr slide-module-file)) (car slide-module-file)]
		  [else (error 'slideshow
			       "expects at most one module file, given ~a: ~s"
			       (length slide-module-file)
			       slide-module-file)])]))

	(when (or printing? condense?)
	  (set! use-transitions? #f))

	(when printing?
	  (set! use-offscreen? #f)
	  (set! use-prefetch? #f)
	  (set! keep-titlebar? #t))

	(dc-for-text-size
	 (if printing?
	     ;; Make ps-dc%:
	     (let ([pss (make-object ps-setup%)])
	       (send pss set-mode 'file)
	       (send pss set-file
		     (if print-target
			 print-target
			 (if file-to-load
			     (path-replace-suffix (file-name-from-path file-to-load)
						  (if quad-view?
						      "-4u.ps"
						      ".ps"))
			     "untitled.ps")))
	       (send pss set-orientation 'landscape)
	       (parameterize ([current-ps-setup pss])
		 (let ([p (make-object post-script-dc% (not print-target) #f #t #f)])
		   (unless (send p ok?) (exit))
		   (send p start-doc "Slides")
		   (send p start-page)
		   (set!-values (actual-screen-w actual-screen-h) (send p get-size))
		   p)))

	     ;; Bitmaps give same size as the screen:
	     (make-object bitmap-dc% (make-object bitmap% 1 1))))

	(set!-values (use-screen-w use-screen-h)
		     (if no-squash?
			 (if (< (/ actual-screen-w screen-w)
				(/ actual-screen-h screen-h))
			     (values actual-screen-w
				     (floor (* (/ actual-screen-w screen-w) screen-h)))
			     (values (floor (* (/ actual-screen-h screen-h) screen-w))
				     actual-screen-h))
			 (values actual-screen-w actual-screen-h)))))))