(module prefs-file-unit mzscheme
  (require (lib "unitsig.ss")
	   "prefs-file-sig.ss")

  (provide framework:prefs-file@)

  (define framework:prefs-file@
    (unit/sig framework:prefs-file^
      (import)
      (define (get-preferences-filename)
	(build-path (find-system-path 'pref-dir)
		    (case (system-type)
		      [(macos macosx) "MrEd Preferences"]
		      [(windows) "mred.pre"]
		      [else ".mred.prefs"]))))))