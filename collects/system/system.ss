; Always load the standard system with load/cd

(error-print-width 250)

(when (getenv "MREDCOMPILE")
  (load "compsys.ss"))

(define mred:original-output-port (current-output-port))

; Remember this directory
(define mred:system-source-directory (current-directory))
(constant-name 'mred:system-source-directory)

(define mzlib:constant-lib? #t)

(define mred:plt-home-directory
  (let ([plt (getenv "PLTHOME")])
    (or plt
	(let-values ([(base name dir?) 
		      (split-path mred:system-source-directory)])
	  (if (string? base)
	      (let-values ([(base name dir?) (split-path base)])
		(if (string? base)
		    base
		    mred:system-source-directory))
	      mred:system-source-directory)))))
(define plt:home-directory mred:plt-home-directory)
(define mred:constants:plt-home-directory mred:plt-home-directory)

(define mred:app-sig-location #f)
(define mred:app-location (build-path mred:system-source-directory "app.ss"))
(define mred:output-spidey-file #f)


(define mred:default-splash (build-path mred:plt-home-directory
					"icons"
					"mred.gif"))
(define mred:default-splash-title "MrEd")

(let ([libdir
       (let ([try-dir (build-path (current-directory) 'up "mzlib")])
	 (if (directory-exists? try-dir)
	     try-dir
	     (let ([try-dir (build-path (current-directory) 'up "mzscheme" "mzlib")])
	       (if (directory-exists? try-dir)
		   try-dir
		   (let ([try-dir (build-path (current-directory) 'up 'up "mzscheme" "mzlib")])
		     (if (directory-exists? try-dir)
			 try-dir
			 (let* ([v (getenv "PLTHOME")]
				[dir (if v 
					 v 
					 (cond
					  [(eq? wx:platform 'unix)
					   "/usr/local/lib/plt"]
					  [(eq? wx:platform 'windows)
					   "c:\\plt"]
					  [else ; macintosh (there is no good default here)
					   (current-directory)]))])
			   (build-path dir "mzscheme" "mzlib"))))))))])
  (current-library-path libdir))

(require-library "referc.ss")

(load "debug.ss")
(load "splash.ss")

(mred:debug:printf 'startup "mred:plt-home-directory: ~a" mred:plt-home-directory)
(mred:debug:printf 'startup "mred:system-source-directory: ~a" mred:system-source-directory)

(define (load-system)
  (let ([old (current-directory)])

    (when (not (directory-exists? (current-library-path)))
      (wx:message-box "Cannot find the MzScheme libraries." "Error")
      (error 'mrsystem "cannot find the MzScheme libraries"))

    (require-library "corec.ss")
    (require-library "triggerc.ss")

    (let ([p (build-path mred:plt-home-directory "lib" "require.ss")])
      (when (file-exists? p)
	(require-library p)))

    (reference "macros.ss")
    (reference "sig.ss")
    (when mred:app-sig-location
      (load-recent mred:app-sig-location))
    (reference "invoke.ss")))

(define mred:mred-startup-directory #f)

;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null]
	[eval-string (lambda (s) (eval (read (open-input-string s))))]
	[todo null]
	[no-show-splash? #f])
    (lambda args
      (unless mred:mred-startup-directory
	(set! mred:mred-startup-directory (current-directory)))
      (cond
	[(null? args)
	 (current-directory mred:system-source-directory)
	 (unless (or mred:splash-frame no-show-splash?)
	   (mred:open-splash mred:default-splash mred:default-splash-title #f))
	 (let* ([default-path (build-path mred:plt-home-directory "afm")]
		[path-box (box default-path)])
	   (wx:get-resource "MrEd" "afmPath" path-box)
	   (wx:set-afm-path 
	    (if (or (directory-exists? (unbox path-box))
		    (not (directory-exists? default-path)))
		(unbox path-box)
		default-path)))
	 (load-system)
	 (current-library-path (normalize-path (current-library-path)))
	 (set! mred:plt-home-directory (normalize-path mred:plt-home-directory))
	 ;(constant-name 'mred:plt-home-directory)
	 (when (and (eq? wx:platform 'windows))
	   (let ([hd (getenv "HOMEDRIVE")]
		 [hp (getenv "HOMEPATH")])
	     (when (and hd hp)
	       (current-directory (build-path hd hp)))))
	 (user-break-poll-handler wx:check-for-break)

	 (mred:change-splash-message "Command Line...")	 
	 (for-each (lambda (x) (apply (car x) (cdr x))) (reverse todo))
	 
	 (current-directory mred:mred-startup-directory)

	 (mred:invoke)
	 (when (getenv "MREDCOMPILE")
	   (wx:exit))
	 (mred:no-more-splash-messages)
	 (mred:build-spidey-unit)
	 (when mred:non-unit-startup?
	   (set! mred:console (mred:startup)))
	 (for-each mred:edit-file files-to-open)
	 (mred:debug:when 'splash
			  (unless (= mred:splash-max mred:splash-counter)
			    (printf "WARNING: splash max (~a) != splash counter (~a)~n"
				    mred:splash-max mred:splash-counter)))
	 (mred:close-splash)
	 mred:console]
       [else 
	(let* ([arg (car args)]
	       [rest (cdr args)]
	       [wrap
		(lambda (f)
		  (lambda (x)
		    (current-directory mred:mred-startup-directory)
		    (begin0(f x)
			   (current-directory mred:system-source-directory))))]
	       [do-f (wrap load-with-cd)]
	       [do-e (wrap eval-string)]
	       [ep
		(lambda (x)
		  (if (relative-path? x)
		      (build-path mred:mred-startup-directory x)
		      x))]
	       [use-next-arg
		(lambda (f)
		  (if (null? rest)
		      (error "expected another arg after ~a" arg)
		      (begin (f (car rest))
			     (apply mred:initialize (cdr rest)))))]
	       [use-next-2args
		(lambda (f)
		  (if (or (null? rest)
			  (null? (cdr rest)))
		      (error "expected another 2 args after ~a" arg)
		      (begin (f (car rest) (cadr rest))
			     (apply mred:initialize (cddr rest)))))]
	       [use-next-3args
		(lambda (f)
		  (if (or (null? rest)
			  (null? (cdr rest))
			  (null? (cddr rest)))
		      (error "expected another three args after ~a" arg)
		      (begin (f (car rest) (cadr rest) (caddr rest))
			     (apply mred:initialize (cdddr rest)))))])
	  (cond
	   [(string-ci=? "-w" arg)
	    (use-next-arg
	     (lambda (arg)
	       (set! mred:output-spidey-file (ep arg))))]
	   [(string-ci=? "-a" arg)
	    (use-next-2args
	     (lambda (apploc sigloc)
	       (set! mred:app-location (ep apploc))
	       (set! mred:app-sig-location (ep sigloc))))]
	   [(string-ci=? "-f" arg)
	    (use-next-arg
	     (lambda (fn)
	       (set! todo (cons (list do-f fn) todo))))]
	   [(string-ci=? "-p" arg) (use-next-3args mred:open-splash)]
	   [(string-ci=? "-b" arg) 
	    (set! no-show-splash? #t)
	    (apply mred:initialize (cdr args))]
	   [(string-ci=? "-e" arg)
	    (use-next-arg
	     (lambda (s)
	       (set! todo (cons (list do-e s) todo))))]
	   [(string-ci=? "--" arg)
	    (use-next-arg
	     (lambda (fn)
	       (set! files-to-open (cons (ep fn) files-to-open))))]
	   [(or (string-ci=? "-q" arg) 
		(string-ci=? "--no-init-file" arg))
	    (set! mred:load-user-setup? #f)
	    (apply mred:initialize rest)]
	   [(string-ci=? "-nu" arg)
	    (set! todo
		  (cons (list (lambda () (mred:non-unit-startup))) todo))
	    (apply mred:initialize rest)]
	   [else (set! files-to-open (cons (ep arg) files-to-open))
		 (apply mred:initialize rest)]))]))))
