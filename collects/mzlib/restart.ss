
(module restart mzscheme
  (import "cmdline.ss")

  (export restart-mzscheme)

  (define (restart-mzscheme init-argv adjust-flag-table argv init-namespace)
    (let* ([result #t]
	   [args #f]
	   [mute-banner? #f]
	   [no-rep? #f]
	   [no-coll-paths? #f]
	   [no-init-file? #f]
	   [case-sensitive? #f]
	   [esc-cont-only? #f]
	   [allow-set!-undefined? #t]
	   [print-error
	    (lambda (e)
	      (if (exn? e)
		  (fprintf (current-error-port) "~a~n" (exn-message e))
		  (fprintf (current-error-port) "Exception in init file: ~e~n" e)))]
	   [table
	    `([multi
	       [("-e")
		,(lambda (f expr) expr)
		("Evaluates <expr>" "expr")]
	       [("-f")
		,(lambda (f file) (format "(load ~s)" file))
		("Loads <file>" "file")]
	       [("-d")
		,(lambda (f file) (format "(load/cd ~s)" file))
		("Load/cds <file>" "file")]
	       [("-i")
		,(lambda (f file) (format "(import (file ~s))" file))
		("Imports <file>" "file")]
	       [("-F")
		,(lambda (f . files) (map (lambda (file)
					    (format "(load ~s)" file))
					  files))
		("Loads all <file>s" "file")]
	       [("-D")
		,(lambda (f . files) (map (lambda (file)
					    (format "(load/cd ~s)" file))
					  files))
		("Load/cds all <file>s" "file")]
	       [("-I")
		,(lambda (f . files) (map (lambda (file)
					    (format "(import (file ~s))" file))
					  files))
		("Imports all <file>s" "file")]
	       [("-l")
		,(lambda (f file) (format "(import (lib ~s))" file))
		("Imports library <file>" "file")]
	       [("-L")
		,(lambda (f file collection) (format "(import (lib ~s ~s))" file collection))
		("Imports library <file> in <collection>" "file" "collection")]
	       [("-r" "--script")
		,(lambda (f file . rest) 
		   (format "(load ~s)" file)
		   (set! mute-banner? #t)
		   (set! no-rep? #t)
		   (set! args rest))
		("Same as -fmv-" "file" "arg")]
	       [("-t" "--script-cd")
		,(lambda (f file . rest) 
		   (format "(load/cd ~s)" file)
		   (set! mute-banner? #t)
		   (set! no-rep? #t)
		   (set! args rest))
		("Same as -dmv-" "file" "arg")]
	       [("-w" "--awk")
		,(lambda (f) "(require-library \"awk.ss\")")
		("Same as -l awk.ss")]
	       [("-x" "--no-init-path")
		,(lambda (f) (set! no-coll-paths? #t))
		("Don't set current-library-collection-paths")]
	       [("-q" "--no-init-file")
		,(lambda (f) (set! no-init-file? #t))
		("Don't load \"~/.mzschemerc\" or \"mzscheme.rc\"")]
	       [("-g" "--case-sens")
		,(lambda (f) (set! case-sensitive? #t))
		("Identifiers and symbols are initially case-sensitive")]
	       [("-c" "--esc-cont")
		,(lambda (f) (set! esc-cont-only? #t))
		("Call/cc is replaced with call/ec")]
	       [("-s" "--set-undef")
		,(lambda (f) (set! allow-set!-undefined? #t))
		("Set! works on undefined identifiers")]
	       [("-m" "--mute-banner")
		,(lambda (f) (set! mute-banner? #t))
		("Suppresses the startup banner text")]
	       [("-v" "--version")
		,(lambda (f) (set! no-rep? #t))
		("Suppresses the read-eval-print loop")]
	       [("--restore")
		,(lambda (f) (error 'mzscheme "The --restore flag is not supported in this mode"))
		("Not supported")]])])
      (parse-command-line
       "mzscheme"
       argv
       table
       void
       '("ignored"))
      (set! args #f)
      (parse-command-line
       "mzscheme"
       argv
       (adjust-flag-table table)
       (lambda (exprs . rest)
	 (unless (null? rest)
	   (set! args rest))
	 ;(when args (set! rest args))
	 (let ([n (make-namespace
		   (if esc-cont-only? 'call/cc=call/ec 'call/cc!=call/ec))])
	   (thread-wait
	    (thread
	     (lambda ()
	       (current-namespace n)
	       (let ([program (find-system-path 'exec-file)])
		 (read-case-sensitive case-sensitive?)
		 (compile-allow-set!-undefined allow-set!-undefined?)
		 
		 (unless mute-banner? (display (banner)))
		 
		 (eval `(#%define-values (argv) (#%quote ,(if args (list->vector args) (vector)))))
		 (eval `(#%define-values (program) (#%quote ,program)))
		 
		 (current-library-collection-paths
		  (if no-coll-paths?
		      #f
		      (path-list-string->path-list 
		       (or (getenv "PLTCOLLECTS") "")
		       (or
			(ormap
			 (lambda (f) (let ([p (f)]) (and p (directory-exists? p) (list p))))
			 (list
			  (lambda () (let ((v (getenv "PLTHOME")))
				       (and v (build-path v "collects"))))
			  (lambda () (find-executable-path program "collects"))
			  (lambda ()
			    (case (system-type)
			      [(unix beos) "/usr/local/lib/plt/collects"]
			      [(windows) "c:\\plt\\collects"]
			      [else #f]))))
			null)))))
	       
	       (init-namespace)
	       
	       (unless no-init-file?
		 (let ([f (case (system-type)
			    [(unix beos) "~/.mzschemerc"]
			    [else "mzscheme.rc"])])
		   (when (file-exists? f)
		     (with-handlers ([void print-error])
		       (load f)))))
	       
	       (let/ec k
		 (exit-handler
		  (lambda (status)
		    (set! result status)
		    (k #f)))
		 (let/ec escape
		   (for-each
		    (lambda (e)
		      (with-handlers ([void (lambda (e) 
					      (print-error e) 
					      (set! result #f)
					      (escape #f))])
			(eval (read (open-input-string e)))))
		    exprs))
		 (unless no-rep? 
		   (read-eval-print-loop)
		   (set! result #t))))))))
       `("arg"))
      result)))
