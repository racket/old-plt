
(module setup-go mzscheme
  (require (lib "cmdline.ss"))
  (require (lib "unitsig.ss"))
  (require (lib "xml-sig.ss" "xml"))

  (require "option-sig.ss")
  (require "setup-unit.ss")
  (require "option-unit.ss")

  (define-values/invoke-unit/sig setup-option^
    setup:option@)

  (define-values (x-specific-collections x-archives)
    (command-line
     "setup-plt"
     (current-command-line-arguments)
     (once-each
      [("-c" "--clean") "Delete existing compiled files; implies -nxi"
			(clean #t)
                        (make-zo #f)
			(call-install #f)
			(make-launchers #f)]
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
      [("--force") "Treat version mismatches for archives as mere warnings"
                   (force-unpacks #t)]
      [("-a" "--all-users") "Install archives into PLTHOME, not user-specific directory"
                            (current-target-plt-directory-getter
			     (lambda (preferred plthome choices) plthome))]
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

  (require (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")

	   (lib "dynext-sig.ss" "dynext")
	   (lib "dynext-unit.ss" "dynext"))

  (require (lib "sig.ss" "compiler")
	   (lib "option-unit.ss" "compiler")
	   (lib "compiler-unit.ss" "compiler"))

  (invoke-unit/sig
   (compound-unit/sig
    (import (SOPTION : setup-option^))
    (link [launcher : launcher^ (launcher@ dcompile dlink)]
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
