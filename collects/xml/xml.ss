
(module xml mzscheme
  (import (lib "unitsig.ss"))

  (import "xml-sig.ss"
	  "xml-unit.ss")
  
  (define-values/invoke-unit/sig xml^ xml@)

  (export-signature-elements xml^))
