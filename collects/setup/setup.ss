
(module setup mzscheme
  (import (lib "cmdline.ss"))
  (import (lib "unitsig.ss"))

  (import "option-sig.ss")
  (import "setup-unit.ss")
  (import "option-unit.ss")

  (define-values/invoke-unit/sig setup-option^
    setup:option@)

  (define-values (x-specific-collections x-archives)
    (command-line
     "setup-plt"
     (global-defined-value 'argv)
     (once-each
      [("-c" "--clean") "Delete existing compiled files"
			(clean #t)]
      [("-n" "--no-zo") "Do not produce .zo files"
			(make-zo #f)]
      [("-x" "--no-launcher") "Do not produce launcher programs"
			      (make-launchers #f)]
      [("-i" "--no-install") "Do not call collection-specific installers"
			     (call-install #f)]
      [("-e" "--extension") "Produce native code extensions"
			    (make-so #t)]
      [("-v" "--verbose") "See names of compiled files and info printfs"
			  (verbose #t)]
      [("-m" "--make-verbose") "See make and compiler usual messages"
			       (make-verbose #t)]
      [("-r" "--compile-verbose") "See make and compiler verbose messages"
				  (make-verbose #t)
				  (compiler-verbose #t)]
      [("-p" "--pause") "Pause at the end if there are any errors"
			(pause-on-errors #t)]
      [("-l") =>
	      (lambda (flag . collections)
		(map list collections))
	      '("Setup specific <collection>s only" "collection")])
     (=>
      (lambda (collections . archives)
	(values (if (null? collections)
		    null
		    (car collections))
		archives))
      '("archive")
      (lambda (s)
	(display s)
	(printf "If no <archive> or -l <collection> is specified, all collections are setup~n")
	(exit 0)))))

  (specific-collections x-specific-collections)
  (archives x-archives)

  (import (lib "launcher-sig.ss" "launcher"))
  (import (lib "launcher-unit.ss" "launcher"))

  (import (lib "dynext-sig.ss" "dynext"))
  (import (lib "dynext-unit.ss" "dynext"))

  (import (lib "sig.ss" "compiler"))
  (import (lib "option-unit.ss" "compiler"))
  (import (lib "compiler-unit.ss" "compiler"))

  (invoke-unit/sig
   (compound-unit/sig
    (import (SOPTION : setup-option^))
    (link [launcher : launcher-maker^ (launcher@ dcompile dlink)]
	  [dcompile : dynext:compile^ (dynext:compile@)]
	  [dlink : dynext:link^ (dynext:link@)]
	  [dfile : dynext:file^ (dynext:file@)]
	  [option : compiler:option^ (compiler:option@)]
	  [compiler : compiler^ (compiler@
				 option
				 dcompile
				 dlink
				 dfile)]
	  [setup : () (setup@
		       SOPTION
		       compiler
		       option
		       launcher)])
    (export))
   setup-option^))
