(module color mzscheme
  (provide (struct color (red green blue)))
  (define-struct color (red green blue) (make-inspector)))

	   