
(lambda (request failure)
  (case request
    [(name) "DrScheme Jr"]
    [(compile-prefix) '(void)]
    [(compile-omit-files) (list "go.ss" "drscheme-jr.ss")]
    [(mzscheme-launcher-libraries) (list "go.ss")]
    [(mzscheme-launcher-names) (list "DrScheme Jr")]
    [else (failure)]))
