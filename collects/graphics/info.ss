(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what failure)
    (case what
      [(name) "Graphics"]
      [(compile-prefix) `(begin
			   ,(drs 'compile-prefix failure)
			   (require-library "tmacro.ss" "graphics"))]
      [(compile-omit-files) (list "tmacro.ss"
				  "turtles.ss"
				  "graphics.ss")]
      [else (failure)])))

