(lambda (request failure)
  (case request
    [(name) "Help"]
    [(compile-prefix) `(begin
			 (require-library "sig.ss" "mred")
			 (require-library "sig.ss" "help")
			 (require-library "help-raw-sig.ss" "help"))]
    [(compile-omit-files) (list "sig.ss" "search-sig.ss" "help-raw-sig.ss" "manuals.ss")]
    [(compile-elaboration-zos) (list "sig.ss" "search-sig.ss" "help-raw-sig.ss")]
    [(mred-launcher-libraries) (list "help.ss")]
    [(mred-launcher-names) (list "Help Desk")]
    [else (failure)]))

