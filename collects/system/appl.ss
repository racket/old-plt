(let-signature user-setup^ (user-setup?)
  (lambda (console@)
    (let ([cmd-line@
	   (unit/sig user-setup^
	     (import [mred : mred^]
		     mred:application-imports^)

	     (mred:current-app-name "MrEd")
	     (mred:scheme-mode-allow-console-eval #t)
	     (define user-setup? #t)
	     
	     (define table
	       (let ([no-user-setup (lambda () (set! user-setup? #f))]
		     [show-version
		      (lambda ()
			(printf "MrEd version ~a, Copyright (c) 1995-1997 PLT, Rice University~
~n~a               (Matthew Flatt and Robert Bruce Findler)~n" 
				(version) (make-string (string-length (version)) #\space)))]
		     [help
		      (lambda ()
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
~nevaluated/loaded, unless the -q or --no-init-file flag is used.~n"))])
		 
		 (list (cons "-q" no-user-setup)
		       (cons "--no-init-file" no-user-setup)
		       (cons "-e" (lambda (s) (eval (read (open-input-string s)))))
		       (cons "-f" load)
		       (cons "-d" load/cd)
		       (cons "-h" help)
		       (cons "--help" help)
		       (cons "-v" show-version)
		       (cons "--version" show-version)
		       (cons "--" (lambda x (for-each mred:edit-file x))))))
	     
	     (define process-command-line-arguments
	       (lambda (table arguments unknown)
		 (let loop ([n 0])
		   (when (< n (vector-length arguments))
		     (let* ([arg (vector-ref arguments n)]
			    [ass (assoc arg table)])
		       (if ass
			   (let* ([func (cdr ass)]
				  [ar (arity func)])
			     (cond
			       [(number? ar)
				(if (< (+ n ar) (vector-length arguments))
				    (apply func
					   (let loop ([i 0])
					     (cond
					       [(= i ar) null]
					       [(< i ar) (cons (vector-ref arguments (+ i n 1))
							       (loop (+ i 1)))])))
				    (error 'process-command-line-arguments
					   "~a expects ~a arguments, only received ~a"
					   arg ar (- (vector-length arguments) n)))
				(loop (+ n 1 ar))]
			       [(arity-at-least? ar)
				(let ([at-least (arity-at-least-value ar)])
				  (if (< (+ n at-least) (vector-length arguments))
				      (apply func
					     (let loop ([i n])
					       (cond
						 [(= i (vector-length arguments)) null]
						 [else (cons (vector-ref arguments i)
							     (loop (+ i 1)))])))
				      (error 'process-command-line-arguments
					     "~a expects at least ~a arguments, only received ~a"
					     arg at-least (- (vector-length arguments) n))))]
			       [else
				(error 'process-command-line-arguments
				       "found function with varying arity, only allow lambda form, not case-lambda or opt-lambda"
				       )]))
			   (unknown (vector-ref arguments n))))))))

	     (process-command-line-arguments table argv mred:edit-file))]
	  [user-init@
	   (unit/sig ()
	     (import user-setup^)
	     
	     (when user-setup?
	       (let* ([init-file (wx:find-path 'init-file)])
		 (when (file-exists? init-file)
		   (with-handlers ([(lambda (x) #t)
				    (lambda (e) 
				      (wx:message-box (exn-message e)
						      (format "~a Error" init-file)))])
		     (load/cd init-file))))))])
      (compound-unit/sig (import (I : mred:application-imports^))
	(link
	 [flat-mred : ((open mred^) console)
		    ((compound-unit/sig (import [I : mred:application-imports^])
		       (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
			     [mred : mred^ ((reference-library-unit/sig "link.ss" "mred") core)]
			     [cmd-line : user-setup^ (cmd-line@ mred I)]
			     [console : (console) (console@ mred I)]
			     [user-init : () (user-init@ cmd-line)])
		       (export (open mred)
			       (open console)))
		     I)])
	(export (unit flat-mred mred))))))
