(lambda (request failure)
  (case request
    [(name) "stepper"]
    [(compile-prefix) 
     (begin 
       (require-library "sig.ss" "stepper")
       (require-library "drsig.ss" "drscheme"))]
    [else (failure)]))
