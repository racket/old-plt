
(lambda (request failure)
  (case request
    [(name) "Setup PLT"]
    [(compile-prefix) `(begin
			 (require-library "refer.ss")
			 (require-library "plt-installers.ss" "setup")
			 (require-library "setupsig.ss" "setup"))]
    [(compile-omit-files) (list "setup.ss" "setupsig.ss" "plt-installers.ss" "get-infos.ss")]
    [(compile-elaboration-zos) (list "setupsig.ss" "get-infos.ss")]
    [(mzscheme-launcher-libraries) (list "setup.ss")]
    [(mzscheme-launcher-names) (list "Setup PLT")]
    [else (failure)]))
