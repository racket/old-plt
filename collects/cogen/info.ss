(lambda (what failure)
  (case what
    [(name) "Code Generation"]
    [(compile-prefix) '(begin (printf "hi?~n")
			      ;(require-library "loader.ss" "system")
			      (require-library "refer.ss")
			      (require-library "match.ss")
			      (require-library "ariess.ss" "cogen")
			      (require-library "zsigs.ss" "zodiac")
			      (require-library "sigs.ss" "zodiac"))]
    [(compile-omit-files) (list "ariess.ss")]
    [else (failure)]))
