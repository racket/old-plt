; Always load the standard system with load/cd

(error-print-width 250)

(define mred:debug:turned-on (box (list 'load 'startup 'invoke 'html 'dv)))

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
		 (and (> len 4)
		      (string=? (substring x (- len 4) len) "link"))))])
    (current-load (lambda (f)
		    (let ([file (if (relative-path? f)
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

(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:make-mred-invokable-unit
  (lambda ()
    (let ([application (mred:make-application@)])
      (unit/sig->unit
       (compound-unit/sig (import)
	 (link [core : mzlib:core^ (mzlib:core@)]
	       [trigger : mzlib:trigger^ (mzlib:trigger@)]
	       [mred : mred^ (mred@ core trigger application)]
	       [application : mred:application^ (application mred core)])
	 (export (unit mred)
		 (unit application mred)))))))

;; one of these two definitions will be redefined by the application
(define mred:make-invokable-unit mred:make-mred-invokable-unit)
(define mred:make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred@ : mred^]
	      [core@ : mzlib:core^])
      (define app-name "MrEd")
      (define console (make-object mred@:console-frame%))
      (define eval-string (lambda (s)
			    (let ([ce (send console get-edit)])
			      (send ce eval-and-display s)
			      (send ce insert-prompt)
			      #t))))))

(define mred:non-unit-make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred : mred^]
	      [core : mzlib:core^])
      (define app-name "MrEd")
      (define console (make-object wx:frame% '() "hidden"))
      (define eval-string (lambda (string) (void))))))

(define mred:non-unit-startup
  (lambda ()
    (set! mred:non-unit-startup? #t)
    (set! mred:make-application@ mred:non-unit-make-application@)
    (invoke-open-unit (mred:make-invokable-unit) #f)
    (when mred:load-user-setup?
      (mred:user-setup))))

(define mred:startup
  (lambda ()
    (make-object mred:console-frame%)))

;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null])
    (lambda args
      (cond
	[(null? args)
	 (when (and (eq? wx:platform 'windows))
	   (let ([hd (getenv "HOMEDRIVE")]
		 [hp (getenv "HOMEPATH")])
	     (when (and hd hp)
	       (current-directory (build-path hd hp)))))
	 (when (eq? mred:debug:on? 'compile-and-exit)
	   (wx:exit))
	 (user-break-poll-handler wx:check-for-break)
	 (unless mred:non-unit-startup?
	   (invoke-open-unit (mred:make-invokable-unit) #f)
	   (when mred:load-user-setup?
	     (mred:user-setup)))
	 (for-each (lambda (s) (thread (lambda () (mred:edit-file s))))
		   files-to-open)
	 (when mred:non-unit-startup?
	   (set! mred:console (mred:startup)))
	 mred:console]
	[else 
	 (let* ([arg (car args)]
		[rest (cdr args)]
		[use-next-arg
		 (lambda (f)
		   (if (null? rest)
		       (error "expected another arg after ~a" arg)
		       (begin (f (car rest))
			      (apply mred:initialize (cdr rest)))))])
	   (cond
	     [(string-ci=? "-f" arg) (use-next-arg load-with-cd)]
	     [(string-ci=? "-e" arg) (use-next-arg eval-string)]
	     [(string-ci=? "--" arg) (use-next-arg
				      (lambda (fn)
					(set! files-to-open
					      (cons fn files-to-open))))]
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
    (let* ([init-file (build-path (wx:find-directory 'init)
				  (if (eq? wx:platform 'unix)
				      ".mredrc"
				      "mredrc.ss"))])
      (when (file-exists? init-file)
	(let ([orig-escape (error-escape-handler)])
	  (catch-errors (lambda (s) (wx:message-box s "Error"))
			(lambda () (orig-escape))
			(mred:eval-string 
			 (string-append
			  (expr->string `(load/cd ,init-file))))))))))

(when (eq? wx:platform 'unix)
  (let* ([default-path "/usr/local/transcript-4.0/lib/"]
	 [path-box (box default-path)])
    (wx:get-resource "MrEd" "afmPath" path-box)
    (wx:set-afm-path 
     (if (or (directory-exists? (unbox path-box))
	     (not (directory-exists? default-path)))
	 (unbox path-box)
	 default-path))))
