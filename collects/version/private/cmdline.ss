(module cmdline mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "cmdline.ss"))

  (require "checksigs.ss")

  (provide cmdline@)

  (define cmdline@
    (unit/sig args^
      (import progname^)

      (define argv (namespace-variable-binding 'argv))

      (define candidates '())

      (parse-command-line 
       progname
       argv
       `((multi 
	  [("-l")
	   ,(lambda (flag col)
	      (set! candidates
		    (cons col candidates)))
	   ("represents a package" "collection")]))
       (lambda (x) (void))
       '())

      (define collections candidates))))


