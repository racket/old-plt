
(module collection mzscheme
  (import (lib "unitsig.ss"))

  (import (lib "file-sig.ss" "dynext")
	  (lib "file.ss" "dynext")
	  (lib "sig.ss" "compiler")
	  (lib "compiler.ss" "compiler")
	  (lib "option.ss" "compiler"))

  (import "make-sig.ss"
	  "make.ss"
	  "collection-sig.ss"
	  "collection-unit.ss")
  
  (define-values/invoke-unit/sig make:collection^ 
    make:collection@
    #f
    make^
    dynext:file^
    compiler:option^
    compiler^)

  (export-signature-elements make:collection^))
