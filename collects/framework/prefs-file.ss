(module prefs-file mzscheme
  (require "prefs-file-unit.ss"
	   "prefs-file-sig.ss"
	   (lib "unitsig.ss"))

  (define-values/invoke-unit/sig
    framework:prefs-file^
    framework:prefs-file@))