
(load-relative "testing.ss")

(SECTION 'module)

(module n mzscheme (define n 'n) (provide n))

(syntax-test #'(module))
(syntax-test #'(module m))
(syntax-test #'(module 5 mzscheme))

(syntax-test #'(module m 5))

(syntax-test #'(module m mzscheme . 1))

(syntax-test #'(#%module-begin))
(syntax-test #'(+ (#%module-begin) 2))

(syntax-test #'(provide))
(syntax-test #'(provide . x))
(syntax-test #'(module m mzscheme (provide . x)))

(syntax-test #'(require . x))
(syntax-test #'(require m . x))
(syntax-test #'(module m mzscheme (require n . x)))

(syntax-test #'(module m mzscheme (define car 5)))
(syntax-test #'(module m mzscheme (define x 6) (define x 5)))
(syntax-test #'(module m mzscheme (define x 10) (define-syntax x 10)))
(syntax-test #'(module m mzscheme (define-syntax x 10) (define x 10)))

(syntax-test #'(module m mzscheme (provide x)))
(syntax-test #'(module m mzscheme (define x 10) (provide x x)))
(syntax-test #'(module m mzscheme (define x 10) (provide x y)))

;; Cyclic re-def of n:
(syntax-test #'(module n n 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
