; Always load the standard system with load/cd

; Print a little more than MzScheme automatically does:
(error-print-width 100)

(define mred:debug? #f)

(unless (defined? 'mred:startup-print-status)
   (define mred:startup-print-status 
     (if (and (defined? 'mred:debug?)
	      mred:debug?)
	 (lambda (x) (display x) (newline))
	 (lambda (x) #f))))

; Remember this directory
(define mred:system-source-directory (current-directory))
(constant-name 'mred:system-source-directory)

(mred:startup-print-status "Loading mzlib...")
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
					  [(eq? mwx:platform 'windows)
					   "c:\\plt"]
					  [else ; macintosh
					   (current-directory)]))])
			   (build-path dir "mzscheme" "mzlib"))))))))])
  (current-library-path libdir))
(when (not (directory-exists? (current-library-path)))
    (wx:message-box "Cannot find the MzScheme libraries." "Error")
    (error 'mrsystem "cannot find the MzScheme libraries"))
(define mzlib:constant-lib? #t)
(require-library "corec.ss")
(require-library "triggerc.ss")

(define mred:plt-home-directory
  (if (defined? 'mred:plt-home-directory)
      (normalize-path mred:plt-home-directory)
      (let ([plt (getenv "PLTHOME")])
	(if plt
	    (normalize-path plt)
	(let-values ([(base name dir?) 
		      (split-path mred:system-source-directory)])
	  (if (string? base)
	      (let-values ([(base name dir?) (split-path base)])
		(if (string? base)
		    base
		    mred:system-source-directory))
	      mred:system-source-directory))))))
(constant mred:plt-home-directory)

(when mred:debug?
  (define mred:new-console (void))
  (define mred:new-eval (void))
  (define mred:make-new-console
    (lambda ()
      (load "functor-setup.ss")
      (set! mred:new-eval (make-eval 'wx))
      (set! mred:new-console
	    (new-eval `(begin (define mred:system-source-directory ,mred:system-source-directory)
			      (define mred:plt-home-directory ,mred:plt-home-directory)
			      (require-library "sfunctor.ss")
			      (invoke-open-unit ,(sigfunctor->functor mred:main@) mred)
			      (make-object mred:console-frame% #f)))))))


(for-each (lambda (x)
	    (mred:startup-print-status (string-append "Loading " x "..."))
	    (load/cd (string-append x ".ss")))
	  (list "sig" "debug"
		"autoload" "autosave" "canvas" "console" "edit" "exit" 
		"fileutil" "finder" "findstr" "frame" "group" "guiutils" 
		"handler" "icon" "keys" "mcache" "menu" "mode"
		"paren" "project" "sparen" "ssmode"
		(build-relative-path "hyper" "hyper")))

(mred:startup-print-status "Loaded.")

(load "functor-setup.ss")

(mred:startup-print-status "Compounded.")

(define mred:console-frame (void))
(define mred:console-canvas (void))
(define mred:console-edit (void))

(define mred:initialize
  (lambda args
    (if (and (pair? args)
	     (equal? (car args) "-i"))
	(begin
	  (eval-string (cadr args))
	  (apply mred:initialize (cddr args)))
	(invoke-open-functor (sigfunctor->functor mred:main@) mred))))

(define mred:startup
  (lambda args
    (if (null? args)
	(let ([frame (make-object mred:console-frame% #f)])
	  (set! mred:console-frame frame)
	  (set! mred:console-canvas (ivar frame canvas))
	  (set! mred:console-edit (ivar frame edit))
	  frame)
	(let ([arg (car args)]
	      [rest (cdr args)])
	  (cond
	   [(string-ci=? "-f" arg)
	    (if (null? rest)
		(error "expected a filename to load after -f flag")
		(begin (load-with-cd (car rest))
		       (apply mred:startup (cdr rest))))]
	   [(string-ci=? "-e" arg)
	    (if (null? rest)
		(error "expected a string to evaluate after -e flag")
		(begin (eval-string (car rest))
		       (apply mred:startup (cdr rest))))]
	   [(string-ci=? "--" arg) (for-each mred:edit-file rest)]
	   [else (mred:edit-file arg)
		 (apply mred:startup rest)])))))

(define mred:user-setup
  (lambda ()
    (let* ([home (if (eq? wx:platform 'unix)
		     (expand-path "~")
		     "")]
	   [file (string-append home 
				(if (eq? wx:platform 'windows)
				    "mredrc.ss"
				    ".mredrc"))])
      (if (file-exists? file)
	  (let ([orig-escape (error-escape-handler)])
	  (catch-errors
	   (lambda (s)
	     (wx:message-box s "Error"))
	   (lambda ()
	     (orig-escape))
	   (load-with-cd file)))))))

(if (eq? wx:platform 'unix)
    (let* ([default-path "/usr/local/transcript-4.0/lib/"]
	   [path-box (box default-path)])
      (wx:get-resource "MrEd" "afmPath" path-box)
      (wx:set-afm-path 
       (if (or (directory-exists? (unbox path-box))
	       (not (directory-exists? default-path)))
	   (unbox path-box)
	   default-path))))