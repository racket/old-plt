
(module file mzscheme
  (import (lib "unitsig.ss"))

  (import "file-sig.ss")
  (import "file-unit.ss")

  (define-values/invoke-unit/sig dynext:file^
    dynext:file@)

  (export-signature-elements dynext:file^))
