; Always load the standard system with load/cd

(error-print-width 250)

(load "debug.ss")

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

(require-library "filec.ss")
(require-library "cores.ss")
(require-library "triggers.ss")
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

(load-recent "sig")

(define (load-system)
  (let ([old (current-directory)])
    (current-directory mred:system-source-directory)
    (require-library "corec.ss")
    (require-library "triggerc.ss")

    (let ([p (build-path mred:plt-home-directory "lib" "require.ss")])
      (when (file-exists? p)
	(require-library p)))

    (for-each load-recent
	      (list "macros"

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

(define mred:splash-frame #f)
(define mred:default-splash (build-path mred:system-source-directory
				   "splash.gif"))
(define mred:default-splash-title "MrEd")

(define mred:close-splash
  (lambda ()
    (when mred:splash-frame
      (send mred:splash-frame show #f)
      (set! mred:splash-frame #f))))

(define mred:open-splash
  (lambda (filename title)
    (if (file-exists? filename)
	(let* ([len (string-length filename)]
	       [flag (if (<= len 4)
			 wx:const-bitmap-type-default
			 (let ([suffix (substring filename (- len 4) len)])
			   (cond
			    [(string-ci=? ".xpm" suffix) wx:const-bitmap-type-xpm]
			    [(string-ci=? ".xbm" suffix) wx:const-bitmap-type-xbm]
			    [(string-ci=? ".gif" suffix) wx:const-bitmap-type-gif]
			    [(string-ci=? "pict" suffix) wx:const-bitmap-type-pict]
			    [else wx:const-bitmap-type-default])))]
	       [bitmap (make-object wx:bitmap% filename flag)])
	  (if (send bitmap ok?)
	      (let* ([frame (parameterize ([wx:current-eventspace (wx:make-eventspace)])
			      (make-object wx:dialog-box% '() title))]
		     [width (box 0.)]
		     [height (box 0.)]
		     [c-x-offset 0]
		     [c-y-offset 0]
		     [message (make-object wx:message% frame bitmap)]
		     [msg-width (send message get-width)]
		     [msg-height (send message get-height)])
		(wx:display-size width height)
		(set! width (unbox width))
		(set! height (unbox height))
		(send frame set-size 0 0 width height)
		(let-values ([(c-x-offset c-y-offset)
			     (let ([cwidth (box 0.)]
				   [cheight (box 0.)])
			       (send frame get-client-size cwidth cheight)
			       (values (- width (unbox cwidth))
				       (- height (unbox cheight))))])
		  (send* frame
			 (set-size (/ (- (+ width c-x-offset) msg-width) 2)
				   (/ (- (+ height c-y-offset) msg-height) 2)
				   (+ c-x-offset msg-width)
				   (+ c-y-offset msg-height))
			 (show #t))
		  (wx:flush-display) (wx:yield)
		  (set! mred:splash-frame frame)))
	      (printf "WARNING: bad bitmap ~s" filename)))
	(printf "WARNING: bitmap path ~s not found~n" filename))))
	     
;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null]
	[todo null]
	[no-show-splash? #f])
    (lambda args
      (cond
       [(null? args)
	(unless (or mred:splash-frame no-show-splash?)
	  (mred:open-splash mred:default-splash mred:default-splash-title))
	(load-system)
	(when (and (eq? wx:platform 'windows))
	  (let ([hd (getenv "HOMEDRIVE")]
		[hp (getenv "HOMEPATH")])
	    (when (and hd hp)
	      (current-directory (build-path hd hp)))))
	(when (eq? mred:debug:on? 'compile-and-exit)
	  (wx:exit))
	(user-break-poll-handler wx:check-for-break)
	(for-each (lambda (x) (apply (car x) (cdr x))) todo)
	(unless mred:non-unit-startup?
	  (invoke-open-unit (mred:make-invokable-unit) #f)
	  (when mred:load-user-setup?
	    (mred:user-setup)))
	(for-each mred:edit-file files-to-open)
	(when mred:non-unit-startup?
	  (set! mred:console (mred:startup)))
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
	       [use-next-2args
		(lambda (f)
		  (if (or (null? rest)
			  (null? (cdr rest)))
		      (error "expected another arg after ~a" arg)
		      (begin (f (car rest) (cadr rest))
			     (apply mred:initialize (cddr rest)))))])
	  (cond
	   [(string-ci=? "-f" arg) (use-next-arg
				    (lambda (fn)
				      (set! todo
					    (cons (list load-with-cd fn)
						  todo))))]
	   [(string-ci=? "-p" arg) (use-next-2args mred:open-splash)]
	   [(string-ci=? "-b" arg) 
	    (set! no-show-splash? #t)
	    (apply mred:initialize (cdr args))]
	   [(string-ci=? "-e" arg) (use-next-arg
				    (lambda (s)
				      (set! todo
					    (cons (list eval-string s)
						  todo))))]
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

