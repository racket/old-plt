
(module shared mzscheme
  (require-for-syntax (lib "stx.ss" "syntax")
		      (lib "kerncase.ss" "syntax")
		      "include.ss")

  (provide shared)

  (define undefined (letrec ([x x]) x))
  (require (rename mzscheme the-cons cons))
  
  (define-syntax shared
    (lambda (stx)
      (define make-check-cdr #f)
      (include (build-path "private" "shared-body.ss")))))
