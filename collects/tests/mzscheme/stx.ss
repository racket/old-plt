
(unless (defined? 'SECTION)
  (load-relative "testing.ss"))

(SECTION 'stx)

(test #t syntax? (datum->syntax 'hello #f #f))

(define-syntax mcr
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax (begin x))])))

(define s (quote-syntax (mcr 5)))
(define se (expand-once s))

(syntax-case se ()
  [(bg five)
   (let ([bg (syntax bg)]
	 [five (syntax five)])
     (test 'begin syntax-e bg)
     (test 5 syntax-e five)

     (test #t syntax-original? five)
     (test #f syntax-original? bg)

     'ok)])

(test #f syntax-property s 'testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain s, se derived from part of s

(define s (syntax-property (quote-syntax (mcr 5)) 'testing 10))
(define se (expand-once s))

(test 10 syntax-property s 'testing)
(test 10 syntax-property se 'testing)
(test 'mcr syntax-e (syntax-property se 'origin))

(test #t syntax-original? s)
(test #f syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain s, se is part of s

(define-syntax mcr2
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax x)])))

(define s (syntax-property (quote-syntax (mcr2 5)) 'testing 10))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e s))) syntax-e se)

(test 10 syntax-property s 'testing)
(test 10 syntax-property se 'testing)
(test 'mcr2 syntax-e (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructed s, se is part of s, part of s tagged

(define s (syntax-property (with-syntax ([five (syntax-property (quote-syntax 5)
								'testing
								12)])
			     (syntax (mcr2 five)))
			   'testing 10))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e s))) syntax-e se)

(test 10 syntax-property s 'testing)
(test '(12 . 10) syntax-property se 'testing)
(test 'mcr2 syntax-e (syntax-property se 'origin))

(test #f syntax-original? s)
(test #t syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-step macro chain

(define-syntax mcr5
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax x)])))

(define s (quote-syntax (mcr5 (mcr2 5))))
(define se (expand-once (expand-once s)))

(test (syntax-e (cadr (syntax-e (cadr (syntax-e s))))) syntax-e se)

(define (tree-map f)
  (lambda (l)
    (if (pair? l)
	(cons ((tree-map f) (car l))
	      ((tree-map f) (cdr l)))
	(f l))))

(test '(mcr5 . mcr2)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-step macro chain with expansion

(define-syntax mcr7
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (local-expand (syntax x) (list (quote-syntax #%datum)))])))

(define s (quote-syntax (mcr7 (mcr2 5))))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e (cadr (syntax-e s))))) syntax-e se)

(test '(mcr2 . mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with one expansion

(define s (quote-syntax (mcr5 (mcr7 (mcr2 5)))))
(define se (expand-once (expand-once s)))

(test '(mcr2 mcr5 . mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with other expansion

(define s (quote-syntax (mcr7 (mcr5 (mcr2 5)))))
(define se (expand-once s))

(test '((mcr5 . mcr2) . mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #%app, etc.

(define s (syntax-property (quote-syntax (add1 5)) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

(define s (syntax-property (quote-syntax 5) 'testing 10))
(test 10 syntax-property (expand s) 'testing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
