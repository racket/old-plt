
(module zodiac mzscheme
  (import (lib "unitsig.ss"))

  (import "zodiac-sig.ss")
  (import "zodiac-unit.ss")

  (define-values/invoke-unit/sig zodiac^
    zodiac@)

  (export-signature-elements zodiac^))
