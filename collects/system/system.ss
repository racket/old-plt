(error-print-width 250)

(when (getenv "MREDCOMPILE")
  (load-relative "compsys.ss"))

(define mred:explicit-wx? #f)

(define mred:original-output-port (current-output-port))
(define mred:original-output-port-backup mred:original-output-port)
(define mred:original-input-port (current-input-port))
(define mred:original-input-port-backup mred:original-output-port)

; Remember this directory
(define mred:system-source-directory (current-load-relative-directory))
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

(current-library-collection-paths
 (list* (build-path plt:home-directory "mred" "collects")
	(current-library-collection-paths)))

(let* ([libdir
	(ormap (lambda (dir)
		 (and (directory-exists? dir)
		      dir))
	       (list
		;(build-path mred:system-source-directory 'up "collects")
		(build-path mred:plt-home-directory "mzscheme" "collects")
		(build-path mred:system-source-directory 'up "mzscheme" "collects")
		(build-path mred:system-source-directory 'up 'up "mzscheme" "collects")
		(build-path (let ([v (getenv "PLTHOME")])
			      (or v 
				  (cond
				   [(eq? wx:platform 'unix)
				    "/usr/local/lib/plt"]
				   [(eq? wx:platform 'windows)
				    "c:\\plt"]
				   [else ; macintosh (there is no good default here)
				    mred:system-source-directory])))
			    "mzscheme" "collects")))])
  (unless libdir
    (wx:message-box "Cannot find the MzScheme libraries." "Error")
    (error 'mrsystem "cannot find the MzScheme libraries"))
  (current-library-collection-paths
   (cons libdir (current-library-collection-paths))))

(require-library "referc.ss")

(load-relative "debug.ss")
(load-relative "splash.ss")

(mred:debug:printf 'startup "mred:plt-home-directory: ~a" mred:plt-home-directory)
(mred:debug:printf 'startup "mred:system-source-directory: ~a" mred:system-source-directory)
(mred:debug:printf 'startup "current-library-collection-paths: ~s"
		   (current-library-collection-paths))

(define (load-system)

  (require-library "corec.ss")
  (require-library "triggerc.ss")

  (current-library-collection-paths
   (map normalize-path (current-library-collection-paths)))
  (mred:debug:printf 'startup "normalized current-library-collection-paths: ~s"
		     (current-library-collection-paths))

  (reference-library "sig.ss" "mred")
  (when mred:app-sig-location
    (load/use-compiled mred:app-sig-location))
  (reference "invoke.ss"))

(define mred:mred-startup-directory (current-directory))

;; called with the arguments on the command line
(define mred:initialize
  (let ([files-to-open null]
	[eval-string (lambda (s) (eval (read (open-input-string s))))]
	[todo null]
	[no-show-splash? #f])
    (lambda args
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
	 (set! mred:plt-home-directory (normalize-path mred:plt-home-directory))
	 ;(constant-name 'mred:plt-home-directory)

	 (user-break-poll-handler wx:check-for-break)

	 (mred:change-splash-message "Command Line...")	 
	 (for-each (lambda (x) (apply (car x) (cdr x))) (reverse todo))
	 
	 (current-directory mred:mred-startup-directory)
	 (when (and (eq? wx:platform 'windows))
	   (let ([hd (getenv "HOMEDRIVE")]
		 [hp (getenv "HOMEPATH")])
	     (when (and hd hp)
	       (current-directory (build-path hd hp)))))

	 (mred:invoke)
	 (mred:build-spidey-unit)
	 (when mred:non-unit-startup?
	   (set! mred:console (mred:startup)))
	 (mred:no-more-splash-messages)
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
	       [do-f (wrap load)]
	       [do-e (wrap eval-string)]
	       [do-d (wrap load/cd)]
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
	    [(or (string-ci=? "-h" arg) (string-ci=? "--help" arg))
	     (printf "Supported switches: ~
~n  -a <file> <file> : start up a unitized application, ~
~n                     specifing the unit and signature files. ~
~n  -b : cancel the splash screen. ~
~n  -d <file> : load/cd's <file> after MzScheme starts. ~
~n  -e <expr> : Evaluates <expr> after MzScheme starts. ~
~n  -f <file> : Loads <file> after MzScheme starts. ~
~n  -h, --help : Shows this information. ~
~n  -nu : use a non-unititzed startup. ~
~n  -p <image-file> <name> <int> : set the splash screen parameters. ~
~n  -q, --no-init-file : Does n ot load \"~~/.mredrc\". ~
~n  -w <file> : write out a analyzable entry point for an application. ~
~n  -v, --version : print version information and exit
~n  -- : No argument following this switch is used as a switch. ~
~nAll remaining arguments are opened to be edited. ~
~nExpressions/files are evaluated/loaded in the specified order. ~
~nThe file \"~~/.mredrc\" is loaded before any expressions/files are ~
~n evaluated/loaded, unless the -q or --no-init-file flag is used.~n")
	     (exit)]
	    [(or (string-ci=? "-v" arg) (string-ci=? "--version" arg))
	     (printf "MrEd version ~a, Copyright (c) 1995-1997 PLT, Rice University~
~n~a               (Matthew Flatt and Robert Bruce Findler)~n" 
		     (version) (make-string (string-length (version)) #\space))
	     (exit)]
	    [(string-ci=? "-w" arg)
	     (use-next-arg
	      (lambda (arg)
		(set! mred:output-spidey-file (ep arg))))]
	    [(string-ci=? "-a" arg)
	     (use-next-2args
	      (lambda (apploc sigloc)
		(set! mred:app-location (ep apploc))
		(set! mred:app-sig-location (ep sigloc))))]
	    [(string-ci=? "-d" arg)
	     (use-next-arg
	      (lambda (fn)
		(set! todo (cons (list do-d fn) todo))))]
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
