
(parameterize ([use-compiled-file-kinds 'none])
  (require-library "compile.ss" "compiler"))

(parameterize ([use-compiled-file-kinds 'none])
  (require-library "cmdline.ss")
  (require-relative-library "setupsig.ss")
  (require-library "invoke.ss"))

(define-values/invoke-unit/sig setup-option^
  (parameterize ([use-compiled-file-kinds 'none])
    (require-relative-library "setup-optionr.ss")))

(define-values (x-specific-collections x-archives)
  (command-line
   "setup-plt"
   argv
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

(parameterize ([use-compiled-file-kinds (if (clean) 'none (use-compiled-file-kinds))])
  (require-library "sig.ss" "compiler"))

(parameterize ([use-compiled-file-kinds (if (clean) 'none (use-compiled-file-kinds))])
  (invoke-unit/sig
   (compound-unit/sig
    (import (SOPTION : setup-option^))
    (link [string : mzlib:string^ ((require-library "stringr.ss"))]
	  [file : mzlib:file^ ((require-library "filer.ss") string function)]
	  [function : mzlib:function^ ((require-library "functior.ss"))]
	  [compile : mzlib:compile^ ((require-library "compiler.ss"))]
	  [pretty-print : mzlib:pretty-print^ ((require-library "prettyr.ss"))]
	  [launcher : launcher-maker^ ((require-library "launcherr.ss" "launcher") file)]
	  [dcompile : dynext:compile^ ((require-library "compiler.ss" "dynext"))]
	  [dlink : dynext:link^ ((require-library "linkr.ss" "dynext"))]
	  [dfile : dynext:file^ ((require-library "filer.ss" "dynext"))]
	  [option : compiler:option^ ((require-library "optionr.ss" "compiler"))]
	  [info : setup:info^ ((require-relative-library "get-infor.ss"))]
	  [compiler : compiler^ ((require-library "compiler.ss" "compiler")
				 option
				 function
				 pretty-print
				 file
				 string
				 compile
				 dcompile
				 dlink
				 dfile)]
	  [setup : () ((require-relative-library "setupr.ss")
		       soption
		       info
		       file
		       compiler
		       option
		       launcher)])
    (export))
   setup-option^))
