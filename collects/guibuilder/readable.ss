
(module readable mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))

  (define gui-code-snip%
    (class* editor-snip% (readable-snip<%>)
      (inherit get-editor)
      (define/public (read-one-special index source line column position)
	(values (send (get-editor) build-code #t #f)
		1
		#t))
      (super-new)))

  (provide gui-code-snip%))
