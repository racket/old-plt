; Always load the standard system with load/cd

; Print a little more than MzScheme automatically does:
(error-print-width 250)

(define mred:debug:turned-on (box (list 'load 'startup 'invoke)))

(define mred:debug@
  (let* ([debug-env (getenv "MREDDEBUG")])
    (unit (import)
      (export (dprintf printf) exit? on? turn-on turn-off)

      (define on? (and debug-env (string->symbol debug-env)))

      (when on? 
	(print-struct #t))
	
      (define turn-on (lambda (s) (set-box! 
				   (global-defined-value 'mred:debug:turned-on)
				   (cons s (unbox (global-defined-value 'mred:debug:turned-on))))))
      (define turn-off (lambda (s) 
			 (set-box! (global-defined-value 'mred:debug:turned-on)
			       (let loop ([l (unbox (global-defined-value 'mred:debug:turned-on))])
				 (cond
				   [(null? l) null]
				   [else (if (eq? (car l) s)
					     (loop (cdr l))
					     (cons (car l)
						   (loop (cdr l))))])))))

      (define dprintf (if on? 
			  (lambda (tag . args)
			    (when (member tag (unbox (global-defined-value 'mred:debug:turned-on)))
			      (apply printf args)
			      (newline)))
			  (lambda args (void))))
      (define exit? #t))))

(invoke-open-unit mred:debug@ mred:debug)

(when mred:debug:on?
  (let ([old-handler (current-load)])
    (current-load (lambda (f)
		    (let ([file (if (relative-path? f)
				    (build-path (current-directory) f)
				    f)]
			  [continue (lambda (y)
				      (mred:debug:printf 'load "Loading ~a..." y)
				      (old-handler y))])
		      (if (and (eq? mred:debug:on? 'compile)
			       (string=? ".ss" (substring file
							  (- (string-length file) 3)
							  (string-length file))))
			(begin
			  (let* ([zo (string-append (substring file 0 (- (string-length file) 3)) ".zo")]
				 [compiled? (and (or (not (file-exists? zo))
						     (<= (file-modify-seconds zo)
							 (file-modify-seconds file)))
						 (begin (mred:debug:printf 'load "Compiling ~a..." file)
							(with-handlers ((void 
									 (lambda (e) 
									   (delete-file zo)
									   ((error-display-handler)
									    (exn-message e))
									   #f)))
							  (compile-file file zo)
							  #t)))])
			    (if compiled?
				(continue zo)
				(continue file))))
			(continue file)))))))

(define mred:debug:new-eval (void))
(define mred:debug:new-console (void))  
(define mred:debug:carryover-ids 
  (list 'mred:system-source-directory
	'mred:make-mred-invokable-unit
	'mred:debug:turned-on))
(define mred:debug:make-new-console
  (if mred:debug:on? 
      (lambda ()
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
		  (invoke-open-unit ,(mred:make-invokable-unit) mred))))
	mred:debug:new-eval)
      (lambda () (void))))

; Remember this directory
(define mred:system-source-directory (current-directory))
(constant-name 'mred:system-source-directory)

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

(define mzlib:constant-lib? #t)
(require-library "corec.ss")
(require-library "triggerc.ss")

(current-library-path (normalize-path (current-library-path)))

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
(mred:debug:printf 'startup "mred:plt-home-directory: ~a" mred:plt-home-directory)

(let ([p (build-path mred:plt-home-directory "lib" "require.ss")])
  (when (file-exists? p)
    (require-library p)))

(for-each load-recent
	  (list "sig" "macros"

		;; standard system units
		"autoload" "autosave" "canvas" "console" "contfram"
		"contkids" "contpanl" "containr" "edframe"
		"edit" "exit" "exn" "fileutil" "finder" "findstr" "frame"
		"group" "guiutils" 
		"handler" "icon" "keys" "mcache" "menu" "mode"
		"paren" "prefs" "project" "sparen" "ssmode"
		"htmlmode"
		"hypredit" "hyprfram" "hyprdial"

		;; linking code
		"link"))

(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:make-mred-invokable-unit
  (lambda ()
    (let ([application (mred:make-application@)])
      (unit/sig->unit
       (compound-unit/sig (import ())
	 (link [core : mzlib:core^ (mzlib:core@)]
	       [trigger : mzlib:trigger^ (mzlib:trigger@)]
	       [mred : mred^ (mred@ core trigger application)]
	       [application : mred:application^ (application mred core)])
	 (export (open mred)
		 (open application)))))))

;; one of these two definitions will be redefined by the application
(define mred:make-invokable-unit mred:make-mred-invokable-unit)
(define mred:make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred@ : mred^]
	      [core@ : mzlib:core^])
      (define console (make-object mred@:console-frame%))
      (define eval-string (ivar (ivar console edit) do-eval)))))

(define mred:non-unit-make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred : mred^]
	      [core : mzlib:core^])
      (define console (make-object wx:frame% '() "hidden"))
      (define eval-string (lambda (string) (void))))))

(define mred:non-unit-startup
  (lambda ()
    (set! mred:non-unit-startup? #t)
    (set! mred:make-application@ mred:non-unit-make-application@)
    (invoke-open-unit (mred:make-invokable-unit) mred)
    (when mred:load-user-setup?
      (mred:user-setup))))

(define mred:startup
  (lambda ()
    (make-object mred:console%)))

;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null])
    (lambda args
      (cond
	[(null? args)
	 (unless mred:non-unit-startup?
	   (invoke-open-unit (mred:make-invokable-unit) mred)
	   (when mred:load-user-setup?
	     (mred:user-setup)))
	 (for-each (lambda (s) (thread (lambda () (mred:edit-file s))))
		   files-to-open)
	 (when mred:non-unit-startup?
	   (set! mred:console (mred:startup)))
	 mred:console]
	[else 
	 (let ([arg (car args)]
	       [rest (cdr args)])
	   (cond
	     [(string-ci=? "-f" arg)
	      (if (null? rest)
		  (error "expected a filename to load after -f flag")
 		  (begin (mred:debug:printf (format "Loading: ~a" (car rest)))
			 (load-with-cd (car rest))
			 (apply mred:initialize (cdr rest))))]
	     [(string-ci=? "-e" arg)
	      (if (null? rest)
		  (error "expected a string to evaluate after -e flag")
		  (begin (eval-string (car rest))
			 (apply mred:initialize (cdr rest))))]
	     [(string-ci=? "--" arg) (for-each mred:edit-file rest)]
	     [(or (string-ci=? "-q" arg) 
		  (string-ci=? "--no-init-file" arg))
	      (set! mred:load-user-setup? #f)
	      (apply mred:initialize rest)]
	     [(string-ci=? "-nu" arg)
	      (mred:non-unit-startup)
	      (mred:debug:printf 'startup "Non-unit startup")
	      (apply mred:initialize rest)]
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
      (when (file-exists? file)
	(let ([orig-escape (error-escape-handler)])
	  (catch-errors (lambda (s) (wx:message-box s "Error"))
			(lambda () (orig-escape))
			(load/cd file)))))))

(when (eq? wx:platform 'unix)
  (let* ([default-path "/usr/local/transcript-4.0/lib/"]
	 [path-box (box default-path)])
    (wx:get-resource "MrEd" "afmPath" path-box)
    (wx:set-afm-path 
     (if (or (directory-exists? (unbox path-box))
	     (not (directory-exists? default-path)))
	 (unbox path-box)
	 default-path))))
