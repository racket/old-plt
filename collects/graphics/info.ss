(lambda (what failure)
  (case what
    [(name) "Graphics"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "graphics.ss" "graphics")
			 (require-library "turtles.ss" "graphics"))]
    [(compile-omit-files) (list "tmacro.ss"
				"turtles.ss"
				"graphics.ss")]
    [else (failure)]))

