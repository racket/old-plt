(module prefs-file mzscheme
  (require "prefs-file-unit.ss"
	   "prefs-file-sig.ss"
	   (lib "unitsig.ss"))

  (provide-signature-elements framework:prefs-file^)

  (define-values/invoke-unit/sig
    framework:prefs-file^
    framework:prefs-file@))