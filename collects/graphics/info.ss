(let ([graphics-info
       (lambda (what failure)
	 (case what
	   [(name) "Graphics"]
	   [(compile-prefix) '(begin
				(require-library "refer.ss")
                                (require-library "functios.ss")
                                (require-library "maths.ss")
				(require-library "sig.ss" "mred")
				(require-library "graphics.ss" "graphics")
				(require-library "turtles.ss" "graphics")
				(require-library "value-turtles.ss" "graphics")
				(require-library "tmacro.ss" "graphics"))]
	   [(compile-omit-files) (list "tmacro.ss"
				       "turtles.ss"
                                       "value-turtles.ss"
				       "graphics.ss")]
	   [else (failure)]))])
  graphics-info)

