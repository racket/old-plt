

(module base64 mzscheme
  (import (lib "unitsig.ss"))

  (import "base64-sig.ss")
  (import "base64-unit.ss")

  (define-values/invoke-unit/sig net:base64^
    net:base64@)

  (export-signature-elements net:base64^))
