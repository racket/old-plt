(lambda (request failure)
  (case request
    [(name) "stepper-graphical"]
    [(compile-prefix) 
     '(begin 
        (require-library "sig.ss" "stepper")
        (require-library "drsig.ss" "drscheme"))]
    [else (failure)]))
