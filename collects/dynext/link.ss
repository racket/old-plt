
(module link mzscheme
  (import (lib "unitsig.ss"))

  (import "link-sig.ss")
  (import "link-unit.ss")

  (define-values/invoke-unit/sig dynext:link^
    dynext:link:unit)

  (export-signature-elements dynext:link^))
