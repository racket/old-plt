
(lambda (request failure)
  (case request
    [(name) "DrScheme Jr"]
    [(compile-prefix) '(void)]
    [(compile-omit-files) (list "drscheme-jr.ss")]
    [(mzscheme-launcher-libraries) (list "drscheme-jr.ss")]
    [(mzscheme-launcher-names) (list "DrScheme Jr")]
    [else (failure)]))
