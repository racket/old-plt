; Always load the standard system with load/cd

(error-print-width 250)

(load (if (file-exists? "splash.zo")
	  "splash.zo"
	  "splash.ss"))

(define mred:original-output-port (current-output-port))

; Remember this directory
(define mred:system-source-directory (current-directory))
(constant-name 'mred:system-source-directory)

(define mred:default-splash (build-path mred:system-source-directory
				   "splash.gif"))
(define mred:default-splash-title "MrEd")

(define mzlib:constant-lib? #t)

(define mred:plt-home-directory
  (and (defined? 'mred:plt-home-directory)
       mred:plt-home-directory))


(define (load-system)
  (let ([old (current-directory)])

    (set! mred:plt-home-directory
	  (or mred:plt-home-directory
	      (let ([plt (getenv "PLTHOME")])
		(if plt
		    plt
		    (let-values ([(base name dir?) 
				  (split-path mred:system-source-directory)])
		      (if (string? base)
			  (let-values ([(base name dir?) (split-path base)])
			    (if (string? base)
				base
				mred:system-source-directory))
			  mred:system-source-directory))))))

    (current-directory mred:system-source-directory)
    (load "debug.ss")

    (mred:debug:printf 'startup "mred:plt-home-directory: ~a" mred:plt-home-directory)

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
					      [else ; macintosh
					       (current-directory)]))])
			       (build-path dir "mzscheme" "mzlib"))))))))])
      (current-library-path libdir))
    (when (not (directory-exists? (current-library-path)))
      (wx:message-box "Cannot find the MzScheme libraries." "Error")
      (error 'mrsystem "cannot find the MzScheme libraries"))

    (require-library "corec.ss")
    (require-library "triggerc.ss")

    (let ([p (build-path mred:plt-home-directory "lib" "require.ss")])
      (when (file-exists? p)
	(require-library p)))

    (for-each load-recent
	      (list "macros" "sig" "invoke"

		    ;; standard system units
		    "autoload" "autosave" "canvas" "connect"
		    "console" "contfram"
		    "contkids" "contpanl" "containr" "edframe"
		    "edit" "exit" "exn" "fileutil" "finder" "findstr" "frame"
		    "group" "guiutils" "handler"
		    "html" "hypredit" "hyprfram" "hyprdial"
		    "icon" "keys" "mcache" "menu" "mode" "panel" "paren"
		    "prefs" "project" "sparen" "ssmode" "url" "version"

		    ;; linking code
		    "link"))
    (current-directory old)))

;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null]
	[eval-string (lambda (s) (eval (read (open-input-string s))))]
	[todo null]
	[no-show-splash? #f])
    (lambda args
      (cond
       [(null? args)
	(unless (or mred:splash-frame no-show-splash?)
	  (mred:open-splash mred:default-splash mred:default-splash-title #f))
	(when (eq? wx:platform 'unix)
	  (let* ([default-path "/usr/local/transcript-4.0/lib/"]
		 [path-box (box default-path)])
	    (wx:get-resource "MrEd" "afmPath" path-box)
	    (wx:set-afm-path 
	     (if (or (directory-exists? (unbox path-box))
		     (not (directory-exists? default-path)))
		 (unbox path-box)
		 default-path))))
	(load-system)
	(current-library-path (normalize-path (current-library-path)))
	(set! mred:plt-home-directory (normalize-path mred:plt-home-directory))
	(constant-name 'mred:plt-home-directory)
	(when (and (eq? wx:platform 'windows))
	  (let ([hd (getenv "HOMEDRIVE")]
		[hp (getenv "HOMEPATH")])
	    (when (and hd hp)
	      (current-directory (build-path hd hp)))))
	(when (eq? mred:debug:on? 'compile-and-exit)
	  (wx:exit))
	(user-break-poll-handler wx:check-for-break)
	(mred:change-splash-message "Command Line...")
	(for-each (lambda (x) (apply (car x) (cdr x))) (reverse todo))
	(mred:invoke)
	(mred:no-more-splash-messages)
	(when mred:non-unit-startup?
	  (set! mred:console (mred:startup)))
	(for-each mred:edit-file files-to-open)
	(when mred:debug:on?
	  (unless (= mred:splash-max mred:splash-counter)
	    (printf "WARNING: splash max (~a) != splash counter (~a)~n"
		    mred:splash-max mred:splash-counter)))
	(mred:close-splash)
	mred:console]
       [else 
	(let* ([arg (car args)]
	       [rest (cdr args)]
	       [use-next-arg
		(lambda (f)
		  (if (null? rest)
		      (error "expected another arg after ~a" arg)
		      (begin (f (car rest))
			     (apply mred:initialize (cdr rest)))))]
	       [use-next-3args
		(lambda (f)
		  (if (or (null? rest)
			  (null? (cdr rest))
			  (null? (cddr rest)))
		      (error "expected another three args after ~a" arg)
		      (begin (f (car rest) (cadr rest) (caddr rest))
			     (apply mred:initialize (cdddr rest)))))])
	  (cond
	   [(string-ci=? "-f" arg) (use-next-arg
				    (lambda (fn)
				      (set! todo
					    (cons (list load-with-cd fn)
						  todo))))]
	   [(string-ci=? "-p" arg) (use-next-3args mred:open-splash)]
	   [(string-ci=? "-b" arg) 
	    (set! no-show-splash? #t)
	    (apply mred:initialize (cdr args))]
	   [(string-ci=? "-e" arg)
	    (use-next-arg
	     (lambda (s)
	       (set! todo (cons (list eval-string s) todo))))]
	   [(string-ci=? "--" arg) (use-next-arg
				    (lambda (fn)
				      (set! files-to-open
					    (cons fn files-to-open))))]
	   [(or (string-ci=? "-q" arg) 
		(string-ci=? "--no-init-file" arg))
	    (set! mred:load-user-setup? #f)
	    (apply mred:initialize rest)]
	   [(string-ci=? "-nu" arg)
	    (set! todo
		  (cons (list (lambda () (mred:non-unit-startup))) todo))
	    (apply mred:initialize rest)]
	   [else (set! files-to-open (cons arg files-to-open))
		 (apply mred:initialize rest)]))]))))


