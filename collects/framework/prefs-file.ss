(module prefs-file mzscheme
  (require "prefs-file-unit.ss"
	   "prefs-file-sig.ss"
	   (lib "unitsig.ss"))

  (provide-signature-elements ((unit prefs-file : framework:prefs-file^)))

  (define-values/invoke-unit/sig
    ((unit prefs-file : framework:prefs-file^))
    (compound-unit/sig
     (import)
     (link [prefs-file : framework:prefs-file^ (framework:prefs-file@)])
     (export (unit prefs-file)))))
