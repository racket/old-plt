;(load-relative "loader.ss")

(when (getenv "MREDCOMPILE")
  (load-relative "compsys.ss"))

(error-print-width 250)
(read-case-sensitive #t) ;; reminder, more than anything
(require-library "refer.ss")
(load-relative "invsig.ss")
(load-relative "debug.ss")

(require-library "cmdlines.ss")

(mred:debug:printf 'startup "current-library-collection-paths: ~s"
		   (current-library-collection-paths))

;; called with the arguments on the command line
(define mred:initialize
  (let* ([splash/invoke
	  (unit/sig (mred:startup-application)
	    (import [wx : wx^])
	    (include "splash.ss")
	    (include "invoke.ss"))]
	 [cmdline-table
	  (unit/sig (output-spidey-file table info app-collection)
	    (import)

	    (define output-spidey-file #f)
	    (define app-determined? #f)
	    (define app-name "MrEd-n")
	    (define app-collection "system")
	    (define app-unit-library "nuapp.ss")
	    (define app-sig-library "nusig.ss")
	    (define splash-path (with-handlers ([(lambda (x) #t)
						 (lambda (x) #f)])
				  (build-path (collection-path "icons") "mred.gif")))
	    (define splash-depth 4)
	    (define splash-max 73)
	    (define config-info
	      (lambda (sym failure)
		(case sym
		  [(app-unit-library) app-unit-library]
		  [(app-sig-library) app-sig-library]
		  [(name) app-name]
		  [(splash-image-path) splash-path]
		  [(splash-depth) splash-depth]
		  [(splash-max) splash-max]
		  [else (failure)])))

	    (define info (require-library "info.ss" "system"))

	    (define table
	      `((once-any
		 [("--version" "-v")
		  ,(lambda (_)
		     (printf "MrEd version ~a, Copyright (c) 1995-1997 PLT, Rice University~
~n~a               (Matthew Flatt and Robert Bruce Findler)~n" 
			     (version) (make-string (string-length (version)) #\space))
		     (exit))
		  ("Print version information")]
		 [("-A")
		  ,(lambda (_ collection)
		     (set! app-collection collection)
		     (set! info (require-library "info.ss" collection)))
		  ("Use collection's info.ss for application"
		   "collection")]
		 [("-a")
		  ,(lambda (_ collection unit-lib sig-lib)
		     (set! app-collection collection)
		     (set! app-unit-library unit-lib)
		     (set! app-sig-library sig-lib)
		     (set! info config-info)
		     '-a)
		  ("Use collection's unit file and sig/macros file for application"
		   "collection"
		   "unit file"
		   "sig/macros file")]
		 [("-n")
		  ,(lambda (_)
		     (set! info config-info)
		     '-n)
		  ("Non unitized application")])
		(once-each
		 [("-w")
		  ,(lambda (_ arg)
		    (set! output-spidey-file arg))
		  ("Output spidey file to filename"
		   "filename")]
		 [("-b") 
		  ,(lambda (_)
		    (set! splash-path #f)
		    'splash)
		  ("No splash screen (only use with -n or -a)")]
		 [("-p")
		  ,(lambda (_ image title num-files depth)
		     (set! splash-path image)
		     (set! app-name title)
		     (let ([max (string->number num-files)]
			   [deep (string->number depth)])
		       (if (and (number? max)
				(number? deep))
			   (begin
			     (set! splash-max max)
			     (set! splash-depth deep))
			   (error 'startup 
				  "-p flag expects the 3rd and 4th arguments to be numbers, got ~a ~a"
				  num-files depth)))
		     'splash)
		  ("Specify splash screen image, frame title, number of files and depth(only use with -n or -a)"
		   "image"
		   "title"
		   "files"
		   "depth")]))))]
	 [main
	  (unit/sig ()
	    (import (output-spidey-file table info app-collection)
		    (input-args)
		    (mred:startup-application)
		    [wx : wx^]
		    mzlib:command-line^)
	    (define leftover-args null)
	    (parse-command-line "mred"
				(list->vector input-args)
				table
				(lambda (accum . rest)
				  (when (member 'splash accum)
				    (unless (or (member '-a accum)
						(member '-n accum))
				      (error 'commmand-line
					     "found -b or -p and no -a or -n")))
				  (set! leftover-args rest))
				'("arg")
				(lambda (string)
				  (display string)
				  (printf "The remaining arguments are passed on to the application.~n")
				  (printf "Use -- to pass arguments to the application. For example,~n")
				  (printf "use \"mred -- -h\" to see the application's help~n")
				  (exit)))

	    ;; why isn't just one wx:yield neccessary?
	    (sleep) (sleep) (sleep)
	    (wx:yield) (wx:yield) (wx:yield)

	    (when output-spidey-file
	      ((load-relative "spidey.ss") output-spidey-file
					   app-collection
					   info))
	    (mred:startup-application app-collection
				      info
				      leftover-args
				      void))]
	 [compound
	  (compound-unit/sig (import (I : (input-args)))
	    (link [wx : wx^ (wx@)]
		  [splash/invoke : (mred:startup-application) (splash/invoke wx)]
		  [cmdline-table : (output-spidey-file table info app-collection)
				 (cmdline-table)]
		  [cmdline : mzlib:command-line^
			   ((reference-library-unit/sig "cmdliner.ss"))]
		  [main : () (main cmdline-table I splash/invoke wx cmdline)])
	    (export))])
    (lambda input-args
      (invoke-unit/sig compound (input-args)))))
