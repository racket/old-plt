
(module toplevel mzscheme
  (require "kerncase.ss")

  (provide eval-compile-time-part-of-top-level)

  (define (eval-compile-time-part-of-top-level stx)
    (kernel-syntax-case stx #f
      [(begin e ...)
       (for-each eval-compile-time-part-of-top-level (cdr (syntax->list stx)))]
      [(require req ...)
       (for-each (lambda (req)
		   (namespace-require/expansion-time (syntax-object->datum req)))
		 (syntax->list (syntax (req ...))))]
      [(module . _)
       (eval stx)]
      [(define-syntaxes . _)
       (eval stx)]
      [(require-for-syntax . _)
       (eval stx)]
      [(define-values (id ...) . _)
       (for-each (lambda (id)
		   (with-syntax ([id id]
				 [undefined (letrec ([x x]) x)])
		     (eval (syntax (define (id) undefined)))))
		 (syntax->list (syntax (id ...))))]
      [_else (void)])))
