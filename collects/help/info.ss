(lambda (request failure)
  (case request
    [(name) "Help"]
    [(compile-prefix) `(begin
			 (require-library "sig.ss" "mred")
			 (require-library "sig.ss" "help"))]
    [(compile-omit-files) (list "sig.ss" "manuals.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]
    [(mred-launcher-libraries) (list "help.ss")]
    [(mred-launcher-names) (list "Help Desk")]
    [(mzscheme-launcher-libraries) (list "help-raw.ss")]
    [(mzscheme-launcher-names) (list "Raw Help Desk")]
    [else (failure)]))

