(lambda (request failure)
  (case request
    [(name) "MrSpidey"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "sigs.ss" "mrspidey" "Sba")
			 (require-library "sigs.ss" "mrspidey" "Gui"))]
    [(compile-omit-files)
     (list "pltrc-co.ss" "macros.ss"
	   "handlers.ss" "mred.ss" "text.ss")]
    [(compile-subcollections) (list (list "Sba")
				    (list "Gui"))]
    [else (failure)]))
