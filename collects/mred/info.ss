(lambda (request failure)
  (case request
    [(name) "MrEd"]
    [(compile-prefix) '(void)]
    [(compile-omit-files) (list "info.ss" "sig.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]
    [else (failure)]))
