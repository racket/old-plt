
(module compile mzscheme
  (import (lib "unitsig.ss"))
  
  (import "sig.ss")

  (import (lib "compile-sig.ss" "dynext"))
  (import (lib "link-sig.ss" "dynext"))
  (import (lib "file-sig.ss" "dynext"))
  ;;
  (import (lib "compile.ss" "dynext"))
  (import (lib "link.ss" "dynext"))
  (import (lib "file.ss" "dynext"))

  (import "option.ss")

  (import "compiler-unit.ss")

  (define-values/invoke-unit/sig compiler^
    compiler@
    #f
    compiler:option^
    dynext:compile^
    dynext:link^
    dynext:file^)

  (export-signature-elements compiler^))

