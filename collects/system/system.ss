; Always load the standard system with load/cd

; Print a little more than MzScheme automatically does:
(error-print-width 250)

(define mred:debug:turned-on (box (list 'startup 'invoke)))

(define mred:debug@
  (let* ([debug-env (getenv "MREDDEBUG")])
    (unit (import)
      (export (dprintf printf) exit? on?
	       turn-on turn-off new-console
	       new-eval make-new-console)

      (define on? (and debug-env (string=? debug-env "on")))

      (when on? (print-struct #t))

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
      (define exit? #t)
	
      (define new-console (void))
      (define new-eval (void))
      (define make-new-console
	(if on? 
	    (lambda ()
	      (set! new-eval (make-eval 'wx))
	      ((global-defined-value 'mred:make-mred@))
	      (set! new-console 
		    (new-eval `(begin
				 (define q (lambda () (send mred:console show #f)))
				 (define mred:debug:turned-on ,(global-defined-value 'mred:debug:turned-on))
				 (define mred:system-source-directory 
				   ,(global-defined-value 'mred:system-source-directory))
				 (require-library "core.ss")
				 (require-library "pconvert.ss")
				 (load (build-path mred:system-source-directory "sig.ss"))
				 (invoke-open-unit ,((global-defined-value 'mred:make-invokable-unit))
						 mred))))
	      new-eval)
	    (lambda () (void)))))))

(invoke-open-unit mred:debug@ mred:debug)

; Remember this directory
(define mred:system-source-directory (current-directory))
(constant-name 'mred:system-source-directory)

(mred:debug:printf 'startup "Loading mzlib...")
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

(define mred:load-zo/ss
  (lambda (x)
    (let/ec k
      (let* ([ss-file (string-append x ".ss")]
	     [zo-file (string-append x ".zo")]
	     [file (cond
		    [(and (not (file-exists? ss-file))
			  (not (file-exists? zo-file)))
		     (printf "WARNING: cannot find: ~a or ~a.~n" ss-file zo-file)
		     (k #f)]
		    [(not (file-exists? ss-file)) zo-file]
		    [(not (file-exists? zo-file)) ss-file]
		    [(<= (file-modify-seconds ss-file)
			 (file-modify-seconds zo-file))
		     zo-file]
		    [else ss-file])])
	(mred:debug:printf 'startup "Loading ~a..." file)
	(load/cd file)
	#t))))

(for-each mred:load-zo/ss
	  (list "sig" "macros" 
		"autoload" "autosave" "canvas" "console" "contfram"
		"contkids" "contpanl" "containr" "edframe"
		"edit" "exit" "exn" "fileutil" "finder" "findstr" "frame"
		"group" "guiutils" 
		"handler" "icon" "keys" "mcache" "menu" "mode"
		"paren" "prefs" "project" "sparen" "ssmode"
		"hypredit" "hyprfram" "hyprdial"))

(mred:debug:printf 'startup "Loaded.")

(define mred@ (void))
(define mred:make-mred@
  (lambda ()
    (let* ([debug/s@ (unit->unit/sig mred:debug@ () mred:debug^)]
	   [mred:plt-home-directory mred:plt-home-directory]
	   [mred:system-source-directory mred:system-source-directory]
	   [constants@ (unit/sig mred:constants^ (import)
		         (define plt-home-directory mred:plt-home-directory)
			 (define system-source-directory mred:system-source-directory))])
      (set! mred@
	    (compound-unit/sig (import [core : mzlib:core^]
				       [trigger : mzlib:trigger^]
				       [application : mred:application^])
	    (link [constants : mred:constants^ (constants@)]
		  [debug : mred:debug^ (debug/s@)]
		  [exn : mred:exn^ (mred:exn@ debug)]
		  [container : mred:container^
		    (mred:container@ debug (core function@))]
		  [exit : mred:exit^ (mred:exit@ debug preferences gui-utils)]
		  [preferences : mred:preferences^
		    (mred:preferences@ debug exn container exit gui-utils
				       edit (core function@))]
		  [autoload : mred:autoload^
		    (mred:autoload@ debug preferences (core file@))]
		  [autosave : mred:autosave^
		    (mred:autosave@ debug preferences)]
		  [mode : mred:mode^
		    (mred:mode@ debug keymap)]
		  [handler : mred:handler^
		    (mred:handler@ debug group editor-frame finder (core file@))] 
		  [keymap : mred:keymap^
		    (mred:keymap@ debug preferences exit finder handler
				  find-string scheme-paren gui-utils)]
		  [match-cache : mred:match-cache^ (mred:match-cache@ debug)]
		  [scheme-paren : mred:scheme-paren^
		    (mred:scheme-paren@ debug paren)]
		  [paren : mred:paren^ (mred:paren@ debug)]
		  [path-utils : mred:path-utils^ (mred:path-utils@ debug)]
		  [gui-utils : mred:gui-utils^ (mred:gui-utils@ debug (core function@) trigger)]
		  [finder : mred:finder^
		    (mred:finder@ debug preferences (core string@)
				  (core function@) (core file@))]
		  [icon : mred:icon^ (mred:icon@ debug constants)]
		  [menu : mred:menu^ (mred:menu@ debug (core function@))]
		  [edit : mred:edit^ 
		    (mred:edit@ debug finder path-utils mode scheme-paren
				keymap (core function@))]
		  [group : mred:group^ 
		    (mred:group@ debug preferences editor-frame gui-utils
				 exit autosave handler (core function@))]
		  [frame : mred:frame^ 
		    (mred:frame@ debug preferences edit container canvas icon
				 menu group finder handler exit autosave
				 gui-utils (core function@) (core file@))]
		  [canvas : mred:canvas^ 
		    (mred:canvas@ debug container edit (core file@))]
		  [find-string : mred:find-string^ 
		    (mred:find-string@ debug container canvas edit frame)]
		  [editor-frame : mred:editor-frame^ 
		    (mred:editor-frame@ debug preferences edit frame container
					canvas find-string icon menu group
					finder handler exit autosave gui-utils
					(core function@) (core file@))]
		  [project : mred:project^ 
		    (mred:project@ debug group container gui-utils exit finder
				   frame handler (core file@) (core function@))]
		  [console : mred:console^ 
		    (mred:console@ debug preferences edit frame find-string
				   exit finder handler gui-utils scheme-mode
				   scheme-paren (core function@) (core string@) (core pretty-print@) trigger)]
		  [scheme-mode : mred:scheme-mode^ 
		    (mred:scheme-mode@ debug preferences application container
				       mode match-cache paren scheme-paren icon
				       handler keymap (core string@))]
		  [hyper-dialog : mred:hyper-dialog^ 
		    (mred:hyper-dialog@ debug hyper-edit (core file@))]
		  [hyper-edit : mred:hyper-edit^ 
		    (mred:hyper-edit@ debug edit hyper-dialog (core file@)
				      (core string@))]
		  [hyper-frame : mred:hyper-frame^ 
		    (mred:hyper-frame@ debug hyper-edit hyper-dialog container
				       editor-frame canvas group handler)])
	    (export (unit debug)
		    (open constants)
		    (open (exn : mred:exn-external^))
		    (open container) (open preferences)
		    (open autoload) (open autosave) (open exit)
		    (open gui-utils) (open console) (open path-utils)
		    (open finder)
		    (open find-string) (open edit) (open canvas)
		    (open frame) (open editor-frame)
		    (open group) (open handler) (open icon) (open keymap)
		    (open match-cache) (open menu) (open mode) (open project)
		    (open scheme-paren) (open scheme-mode) (open paren)
		    (open hyper-edit) (open hyper-dialog) (open hyper-frame)))))))
(mred:make-mred@)
(mred:debug:printf 'startup "Compounded.")

;; will be redefined by the application
(define mred:make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred@ : mred^]
	      [core@ : mzlib:core^])
      (define console (make-object mred@:console-frame%))
      (define eval-string (ivar (ivar console edit) do-eval)))))

(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:make-invokable-unit
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

(define mred:non-unit-startup
  (lambda ()
    (set! mred:non-unit-startup? #t)
    (set! mred:make-application@
	  (lambda ()
	    (unit/sig mred:application^
	      (import [mred : mred^]
		      [core : mzlib:core^])
	      (define console (make-object wx:frame% '() "hidden"))
	      (define eval-string (lambda (string) (void))))))
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
      (mred:debug:printf 'startup "user-setup; loading ~a..." file)
      (if (file-exists? file)
	  (let ([orig-escape (error-escape-handler)])
	    (catch-errors (lambda (s)
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
