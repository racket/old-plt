(lambda (sym fail)
  (case sym
    [(name) "Launcher"]
    [(blurb)
     (list "Lets you run your program as a stand-alone program, "
	   "with out starting up DrScheme first.")]
    [(help-desk-message)
     (format "Mz/Mr: ~s" `(require-library "launcher.ss" "launcher"))]
    [else (fail)]))
