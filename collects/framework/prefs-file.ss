(unit/sig framework:prefs-file^
  (import)
  
  (define (get-preferences-filename)
    (build-path (find-system-path 'pref-dir)
		(case (system-type)
		  [(macos) "MrEd Preferences"]
		  [(windows) "mred.pre"]
		  [else ".mred.prefs"]))))
