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
(current-library-path "~mflatt/proj/mred/mzscheme/mzlib")
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
      (unless (void? mred:new-console)
	(send mred:new-console show #f))
      (set! mred:new-eval (make-eval 'wx))
      (set! mred:new-console
	    (mred:new-eval `(begin (define mred:system-source-directory ,mred:system-source-directory)
			      (define mred:plt-home-directory ,mred:plt-home-directory)
			      (current-library-path "~mflatt/proj/mred/mzscheme/mzlib")
			      (require-library "unit.ss")
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

(define-signature mred^
  ((open mred:debug^) (open mred:autoload^) (open mred:autosave^)
   (open mred:exit^) (open mred:gui-utils^) (open mred:console^)
   (open mred:path-utils^) (open mred:finder^) (open mred:find-string^)
   (open mred:edit^) (open mred:canvas^) (open mred:frame^)
   (open mred:group^) (open mred:handler^) (open mred:icon^)
   (open mred:keymap^) (open mred:match-cache^) (open mred:menu^)
   (open mred:mode^) (open mred:project^) (open mred:scheme-paren^)
   (open mred:scheme-mode^) (open mred:paren^) (open mred:hyper-edit^)
   (open mred:hyper-dialog^) (open mred:hyper-frame^)))

(define make-mred@
  (lambda ()
    (compound-unit/s mred^
      (import (core mzlib:core^)
	      (trigger mzlib:trigger^)
	      (application mred:application^))
      (link [debug mred:debug^ (mred:debug@)]
	    [autoload mred:autoload^ (mred:autoload@ debug ((core file@)))]
	    [autosave mred:autosave^ (mred:autosave@ debug)]
	    [exit mred:exit^ (mred:exit@ debug)]
	    [mode mred:mode^ (mred:mode@ debug keymap)]
	    [handler mred:handler^ (mred:handler@ debug group frame finder
						((core file@)))] 
	    [keymap mred:keymap^ (mred:keymap@ debug finder handler find-string
					     scheme-paren)]
	    [match-cache mred:match-cache^ (mred:match-cache@ debug)]
	    [scheme-paren mred:scheme-paren^ (mred:scheme-paren@ debug paren)]
	    [paren mred:paren^ (mred:paren@ debug)]
	    [path-utils mred:path-utils^ (mred:path-utils@ debug)]
	    [gui-utils mred:gui-utils^ 
		       (mred:gui-utils@ debug ((core function@)) 
				      trigger)]
	    [finder mred:finder^ (mred:finder@ debug ((core string@))
					     ((core function@)) ((core file@)))]
	    [icon mred:icon^ (mred:icon@ debug)]
	    [menu mred:menu^ (mred:menu@ debug ((core function@)))]
	    [edit mred:edit^ (mred:edit@ debug finder path-utils mode
				       scheme-paren keymap ((core function@)))]
	    [group mred:group^ (mred:group@ debug gui-utils exit autosave
					  handler ((core function@)))]
	    [frame mred:frame^ (mred:frame@ debug edit canvas icon menu
					  group finder handler exit
					  autosave gui-utils
					  ((core function@)) ((core file@)))]
	    [find-string mred:find-string^ (mred:find-string@ debug canvas
							    edit frame)]
	    [canvas mred:canvas^ (mred:canvas@ debug edit ((core file@)))]
	    [project mred:project^ (mred:project@ debug group gui-utils
						exit finder frame handler
						((core file@)) 
						((core function@)))]
	    [console mred:console^
		     (mred:console@ debug edit frame exit finder handler
				  gui-utils scheme-mode scheme-paren
				  ((core function@)) ((core string@))
				  ((core pretty-print@)) 
				  trigger)]
	    [scheme-mode mred:scheme-mode^
			 (mred:scheme-mode@ debug mode match-cache
					  paren scheme-paren icon handler
					  keymap ((core string@)))]
	    [hyper-dialog mred:hyper-dialog^
			  (mred:hyper-dialog@ debug hyper-edit ((core file@)))]
	    [hyper-edit mred:hyper-edit^
			(mred:hyper-edit@ debug edit hyper-dialog
					((core file@)) ((core string@)))]
	    [hyper-frame mred:hyper-frame^
			 (mred:hyper-frame@ debug hyper-edit hyper-dialog
					  frame canvas group handler)])
      (export (open debug) (open autoload) (open autosave) (open exit)
	      (open gui-utils) (open console) (open path-utils) (open finder)
	      (open find-string) (open edit) (open canvas) (open frame)
	      (open group) (open handler) (open icon) (open keymap)
	      (open match-cache) (open menu) (open mode) (open project)
	      (open scheme-paren) (open scheme-mode) (open paren)
	      (open hyper-edit) (open hyper-dialog) (open hyper-frame)))))

(define mred@ (make-mred@))

(mred:startup-print-status "Compounded.")

;; will be redefined by the application
(define mred:make-application@
  (lambda ()
    (unit/s mred:application^
	    (import (mred@ mred^)
		    (core@ mzlib:core^))
	    (define console-frame (make-object mred@:console-frame%))
	    (define console-edit (ivar console-frame edit))
	    (define console-canvas (ivar console-frame canvas))
	    (define eval-string (ivar console-edit do-eval)))))

;; links mred together
(define mred:startup
  (lambda ()
    (let ([application (mred:make-application@)])
      (invoke-open-unit (compound-unit/s ((open mred^)
					  (open mred:application^))
	                  (import)
			  (link [core mzlib:core^ (mzlib:core@)]
				[trigger mzlib:trigger^ (mzlib:trigger@)]
				[mred mred^ (mred@ core trigger application)]
				[application mred:application^
					     (application mred core)])
			  (export (open mred) (open application)))
			mred))))

(define mred:load-user-setup? #t)

;; called with the initialization arguments
(define mred:initialize
  (let ([files-to-open null])
    (lambda args
      (cond
	[(null? args) 
	 (mred:startup)
	 (when mred:load-user-setup?
	   (mred:user-setup))
	 (for-each mred:edit-file files-to-open)]
	[else 
	 (let ([arg (car args)]
	       [rest (cdr args)])
	   (cond
	     [(string-ci=? "-f" arg)
	      (if (null? rest)
		  (error "expected a filename to load after -f flag")
		  (begin (mred:startup-print-status 
			  (format "Loading: ~a" (car rest)))
			 (load-with-cd (car rest))
			 (apply mred:initialize (cdr rest))))]
	     [(string-ci=? "-q" arg)
	      (set! mred:load-user-setup? #f)
	      (apply mred:initialize rest)]
	     [(string-ci=? "-e" arg)
	      (if (null? rest)
		  (error "expected a string to evaluate after -e flag")
		  (begin (eval-string (car rest))
			 (apply mred:initialize (cdr rest))))]
	     [(string-ci=? "--" arg) (for-each mred:edit-file rest)]
	     [else (set! files-to-open (cons arg files-to-open))
		   (apply mred:initialize rest)]))]))))

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

(when (eq? wx:platform 'unix)
  (let* ([default-path "/usr/local/transcript-4.0/lib/"]
	 [path-box (box default-path)])
    (wx:get-resource "MrEd" "afmPath" path-box)
    (wx:set-afm-path 
     (if (or (directory-exists? (unbox path-box))
	     (not (directory-exists? default-path)))
	 (unbox path-box)
	 default-path))))