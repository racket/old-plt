(lambda (what failure)
  (case what
    [(name) "Graphic Userspace"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "graphics.ss" "graphics")
			 (require-library "turtles.ss" "graphics")
			 (require-library "gusrspcs.ss" "gusrspce"))]
      [(compile-omit-files) (list "gusrspcs.ss")]
      [else (failure)]))
