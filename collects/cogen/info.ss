(lambda (what failure)
  (case what
    [(name) "Code Generation"]
    [(compile-prefix) '(begin (require-library "refer.ss")
			      (require-library "sig.ss" "stepper")
			      (require-library "zsigs.ss" "zodiac")
			      (require-library "sigs.ss" "zodiac"))]
    [(compile-omit-files) (list "ariess.ss" "aries-corer.ss")]
    [else (failure)]))
