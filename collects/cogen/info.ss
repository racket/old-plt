(lambda (what failure)
  (case what
    [(name) "Code Generation"]
    [(compile-prefix) '(begin (require-library "refer.ss")
			      (require-library "match.ss")
			      (require-library "ariess.ss" "cogen")
			      (require-library "zsigs.ss" "zodiac")
			      (require-library "sigs.ss" "zodiac"))]
    [(compile-omit-files) (list "ariess.ss")]
    [else (failure)]))
