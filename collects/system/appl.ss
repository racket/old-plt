(lambda (console@)
  (let* ([cmdline-table@
	  (unit/sig (user-setup? argv)
	    (import [mred : mred^]
		    mzlib:command-line^
		    (I : mred:application-imports^))
	    
	    (mred:current-app-name "MrEd")
	    (mred:scheme-mode-allow-console-eval #t)
	    (define user-setup? #t)
	    (define argv null)
	    
	    (define table
	      `((once-any
		 [("-q" "--no-init-file")
		  ,(lambda (_) (set! user-setup? #f))
		  ("Do not load user startup file")])
		(multi
		 [("-e")
		  ,(lambda (_ s) (eval (read (open-input-string s))))
		  ("Evaluate sexp"
		   "sexp")]
		 [("-f")
		  ,(lambda (_ f) (load f))
		  ("Load file"
		   "file")]
		 [("-d")
		  ,(lambda (_ f) (load/cd f))
		  ("Load file using load/cd"
		   "file")])))
	    
	    (parse-command-line "mred"
				I:argv
				table
				(lambda (accum . rest)
				  (set! argv (list->vector rest)))
				'("file")
				(lambda (string)
				  (display string)
				  (printf "All of the <file>s are passed to mred:edit-file~n")
				  (exit))))]
	 [after-console@
	  (unit/sig ()
	    (import [wx : wx^]
		    [mred : mred^]
		    (user-setup? argv))
	    
	    (when user-setup?
	      (let* ([init-file (wx:find-path 'init-file)])
		(when (file-exists? init-file)
		  (with-handlers ([(lambda (x) #t)
				   (lambda (e) 
				     (wx:message-box (exn-message e)
						     (format "~a Error" init-file)))])
		    (load/cd init-file))))))]
	 [inner-compound@
	  (compound-unit/sig (import [I : mred:application-imports^]
				     [core : mzlib:core^]
				     [cmdline : mzlib:command-line^])
	    (link [mred : mred^ ((require-library-unit/sig "link.ss" "mred") core)]
		  [cmdline-table : (user-setup? argv) (cmdline-table@ mred cmdline I)]
		  [console : (console) (console@ mred (cmdline-table : (argv)))]
		  [wx : wx^ (wx@)]
		  [after-console : () (after-console@ wx mred cmdline-table)])
	    (export (open mred)
		    (open console)))])
    (compound-unit/sig 
      (import (I : mred:application-imports^))
      (link
       [wx : wx^ (wx@)]
       [core : mzlib:core^ ((require-library-unit/sig "corer.ss"))]
       [cmdline : mzlib:command-line^ ((require-library-unit/sig "cmdliner.ss"))]
       [flat-mred : ((open mred^) console) (inner-compound@ I core cmdline)])
      (export (unit flat-mred mred)
	      (unit wx)
	      (open (core pretty-print@))
	      (open (core file@))
	      (open (core function@))
	      (open (core compat@))
	      (open (core string@))
	      (open (core compile@))
	      (open (core thread@))
	      (open cmdline)))))
