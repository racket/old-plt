
(module head mzscheme
  (import (lib "unitsig.ss"))

  (import "head-sig.ss")
  (import "head-unit.ss")

  (define-values/invoke-unit/sig net:head^
    net:head@)

  (export-signature-elements net:head^))
