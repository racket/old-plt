(error-print-width 250)

(when (getenv "MREDCOMPILE")
  (load-relative "compsys.ss"))

(define mred:explicit-wx? #f)

(define mred:original-output-port (current-output-port))
(define mred:original-output-port-backup mred:original-output-port)
(define mred:original-input-port (current-input-port))
(define mred:original-input-port-backup mred:original-output-port)

(require-library "refer.ss")

(load-relative "debug.ss")
(load-relative "splash.ss")

(mred:debug:printf 'startup "current-library-collection-paths: ~s"
		   (current-library-collection-paths))

(define (mred:load-system)
  (require-library "core.ss")
  (require-library "trigger.ss")

  (current-library-collection-paths
   (map normalize-path (current-library-collection-paths)))

  (mred:debug:printf 'startup "normalized current-library-collection-paths: ~s"
		     (current-library-collection-paths))

  (reference-library "sig.ss" "mred")
  (reference-library "invoke.ss" "system"))

;; called with the arguments on the command line
(define mred:initialize
  (let ([extra-args null]
	[eval-string (lambda (s) (eval (read (open-input-string s))))]
	[todo null]
	[user-setup? #t]
	[output-spidey-file #f]
	[no-show-splash? #f]
	[app-determined? #f]
	[app-collection "system"]
	[app-unit-library "app.ss"]
	[app-sig-library "sig.ss"]
	[non-unit-startup? #f])
    (lambda args
      (let* ([do-invoke
	      (lambda ()
		(mred:load-system)
		(require-library app-sig-library app-collection)
		(let ([app (mred:make-invokable-unit app-collection app-unit-library)])
		  (mred:change-splash-message "Invoking...")
		  (unless non-unit-startup? (mred:no-more-splash-messages))
		  (let ([argv (list->vector extra-args)])
		    ;; assumes that mred:application-imports^ is only: (argv) 
		    (invoke-open-unit/sig app #f (argv))))
		(when user-setup?
		  (let* ([init-file (wx:find-path 'init-file)])
		    (when (file-exists? init-file)
		      (with-handlers ([void (lambda (e) 
					      (wx:message-box (exn-message e)
							      (format "~a Error" init-file)))])
			(load/cd init-file))))))]
	     [app-determined
	      (lambda ()
		(when app-determined?
		  (error "Conflicting -A, -a, and/or -nu flags"))
		(set! todo (cons (list do-invoke) todo))
		(set! app-determined? #t))])
	(let loop ([args args])
	  (cond
	    [(null? args)
	     (unless no-show-splash? (mred:open-splash app-collection))
	     
	     (unless app-determined?
	       (app-determined))
	     
	     (user-break-poll-handler wx:check-for-break)
	     
	     (mred:change-splash-message "Command Line...")	 
	     (for-each (lambda (x) (apply (car x) (cdr x))) (reverse todo))
	     
	     (when output-spidey-file
	       (mred:build-spidey-unit output-spidey-file
				       app-collection app-unit-library app-sig-library))
	     
	     (let* ([default-path (with-handlers ([void (lambda (x) #f)]) (collection-path "afm"))]
		    [path-box (box (or default-path ""))])
	       (wx:get-resource "MrEd" "afmPath" path-box)
	       (when (directory-exists? (unbox path-box))
		 (wx:set-afm-path (unbox path-box))))
	     
	     (when non-unit-startup?
	       (set! mred:console (apply mred:startup extra-args)))
	     
	     (mred:no-more-splash-messages)
	     (mred:debug:when 'splash
			      (unless (= mred:splash-max mred:splash-counter)
				(printf "WARNING: splash max (~a) != splash counter (~a)~n"
					mred:splash-max mred:splash-counter)))
	     (mred:close-splash)
	     
	     (when (and (eq? wx:platform 'windows))
	       (let ([hd (getenv "HOMEDRIVE")]
		     [hp (getenv "HOMEPATH")])
		 (when (and hd hp)
		   (let ([path (build-path hd hp)])
		     (when (directory-exists? path)
		       (current-directory path))))))]
	    [else 
	     (let* ([arg (car args)]
		    [rest (cdr args)]
		    [do-f load]
		    [do-e eval-string]
		    [do-d load/cd]
		    [use-next-arg
		     (lambda (f)
		       (if (null? rest)
			   (error "expected another arg after ~a" arg)
			   (begin (f (car rest))
				  (loop (cdr rest)))))]
		    [use-next-2args
		     (lambda (f)
		       (if (or (null? rest)
			       (null? (cdr rest)))
			   (error "expected another 2 args after ~a" arg)
			   (begin (f (car rest) (cadr rest))
				  (loop (cddr rest)))))]
		    [use-next-3args
		     (lambda (f)
		       (if (or (null? rest)
			       (null? (cdr rest))
			       (null? (cddr rest)))
			   (error "expected another three args after ~a" arg)
			   (begin (f (car rest) (cadr rest) (caddr rest))
				  (loop (cdddr rest)))))])
	       (cond
		 [(or (string-ci=? "-h" arg) (string-ci=? "--help" arg))
		  (printf "Supported switches: ~
~n  -A <collection> : Invoke a unitized application, using info.ss.~
~n  -a <collection> <file> <file> : Invoke a unitized application, ~
~n                                  specifing the unit and signature files. ~
~n  -b : cancel the splash screen. ~
~n  -d <file> : load/cd's <file> after MzScheme starts. ~
~n  -e <expr> : Evaluates <expr> after MzScheme starts. ~
~n  -f <file> : Loads <file> after MzScheme starts. ~
~n  -h, --help : Shows this information. ~
~n  -nu : Same as -a system nuapp.ss sig.ss. ~
~n  -q, --no-init-file : Does n ot load \"~~/.mredrc\". ~
~n  -w <file> : write out a analyzable entry point for an application. ~
~n  -v, --version : print version information and exit
~n  -- : No argument following this switch is used as a switch. ~
~nAll remaining arguments are delivered to mred:startup; the default~
~nmred:startup opens the specified files for editing. ~
~nExpressions/files/apps are evaluated/loaded/invoked in the specified order. ~
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
		     (set! mred:output-spidey-file arg)))]
		 [(string-ci=? "-A" arg)
		  (app-determined)
		  (use-next-arg
		   (lambda (collection)
		     (let ([info (require-library "info.ss" collection)])
		       (set! app-collection collection)
		       (set! app-unit-library (info 'app-unit-library))
		       (set! app-sig-library (info 'app-sig-library)))))]
		 [(string-ci=? "-a" arg)
		  (app-determined)
		  (use-next-3args
		   (lambda (collection unit-lib sig-lib)
		     (set! app-collection collection)
		     (set! app-unit-library unit-lib)
		     (set! app-sig-library sig-lib)))]
		 [(string-ci=? "-d" arg)
		  (use-next-arg
		   (lambda (fn)
		     (set! todo (cons (list do-d fn) todo))))]
		 [(string-ci=? "-f" arg)
		  (use-next-arg
		   (lambda (fn)
		     (set! todo (cons (list do-f fn) todo))))]
		 [(string-ci=? "-b" arg) 
		  (set! no-show-splash? #t)
		  (loop (cdr args))]
		 [(string-ci=? "-e" arg)
		  (use-next-arg
		   (lambda (s)
		     (set! todo (cons (list do-e s) todo))))]
		 [(string-ci=? "--" arg)
		  (set! extra-args (append extra-args (cdr args)))
		  (loop null)]
		 [(or (string-ci=? "-q" arg) 
		      (string-ci=? "--no-init-file" arg))
		  (set! user-setup? #f)
		  (loop rest)]
		 [(string-ci=? "-nu" arg)
		  (app-determined)
		  (set! non-unit-startup? #t)
		  (set! app-unit-library "nuapp.ss")
		  (apply mred:initialize rest)]
		 [else (set! extra-args (append extra-args (list arg)))
		       (loop rest)]))]))))))
