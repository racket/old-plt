
(load-relative "loadtest.ss")

(SECTION 'stx)

(test #t syntax? (datum->syntax-object #f 'hello #f))

(test #f syntax-line (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test #f syntax-column (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 19 syntax-position (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 'aha syntax-source (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test #f syntax-span (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 88 syntax-span (datum->syntax-object #f 10 '(aha #f #f 19 88)))

(test 7 syntax-line (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 88 syntax-column (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 999 syntax-position (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 'aha syntax-source (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test #f syntax-span (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 22 syntax-span (datum->syntax-object #f 10 '(aha 7 88 999 22)))
(test 0 syntax-span (datum->syntax-object #f 10 '(aha 1 1 1 0)))

(err/rt-test (datum->syntax-object #f 10 10))
(err/rt-test (datum->syntax-object #f 10 '(10)))
(err/rt-test (datum->syntax-object #f 10 '(10 11)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12 13)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12 13 14 15)))
(err/rt-test (datum->syntax-object #f 10 '(a 11.0 12 13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 13 -1)))
(err/rt-test (datum->syntax-object #f 10 '(a 0 12 13 0)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 0 13 0)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 0 0)))

(syntax-test #'quote-syntax)
(syntax-test #'(quote-syntax))
(syntax-test #'(quote-syntax . 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test basic expansion and property propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(test 10 syntax-property (datum->syntax-object #f 0 #f s) 'testing)

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
      [(_ x) (local-expand (syntax x) 'internal-define (list (quote-syntax #%datum)))])))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test module-identifier=? on different phases via syntax-case*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module mta mzscheme
  (define mtax 10)
  (provide mtax))

(module mtb mzscheme
  (define mtby 10)
  (provide mtby))

(module mt1 mzscheme
  (require (prefix a: mta))
  (require-for-syntax (prefix b: mtb))
  (require (prefix mz: mzscheme))

  (define-syntax ck
    (lambda (stx)
      (syntax-case stx ()
	[(_ id et?)
	 (with-syntax ([cmp (if (syntax-e (syntax et?))
				(syntax module-transformer-identifier=?)
				(syntax module-identifier=?))])
	   (syntax
	    (lambda (x)
	      (syntax-case* x (id) cmp
	        [(_ id) #t]
		[else #f]))))])))

  (define has-lam? (ck lambda #f))
  (define has-mz:lam? (ck mz:lambda #f))
  (define has-mtax? (ck a:mtax #f))
  (define has-mtby? (ck b:mtby #f))

  (define has-et-lam? (ck lambda #t))
  (define has-et-mz:lam? (ck mz:lambda #t))
  (define has-et-mtax? (ck a:mtax #t))
  (define has-et-mtby? (ck b:mtby #t))

  (provide has-lam? has-mz:lam? has-mtax? has-mtby?
	   has-et-lam? has-et-mz:lam? has-et-mtax? has-et-mtby?))

(require mt1)
(require-for-syntax mtb)

(test #t has-lam? #'(any lambda))
(test #f has-lam? #'(any lambada))

(test #t has-et-lam? #'(any lambda))
(test #f has-et-lam? #'(any lambada))

;; mz: prefix is there in normal environment:
(test #t has-mz:lam? #'(any lambda))
(test #f has-et-mz:lam? #'(any lambda))
(test #f has-mz:lam? #'(any mz:lambda))
(test #t has-et-mz:lam? #'(any mz:lambda))

;; No mtax anywhere:
(test #f has-mtax? #'(any mtax))
(test #f has-mtax? #'(any a:mtax))
(test #f has-et-mtax? #'(any mtax))
(test #t has-et-mtax? #'(any a:mtax))

;; mtby (without prefix) in trans env
(test #f has-mtby? #'(any mtby))
(test #t has-mtby? #'(any b:mtby))
(test #t has-et-mtby? #'(any mtby))
(test #f has-et-mtby? #'(any b:mtby))

(module mt2 #%kernel
  (require-for-syntax #%kernel)
  (require mt1)
  (require mta)

  ;; For #':
  (define-syntaxes (syntax)
    (lambda (stx)
      (datum->syntax-object
       stx
       (cons
	(quote-syntax quote-syntax)
	(cdr (syntax-e stx)))
       stx)))

  (define-values (run-mt2-test)
    (lambda (test)
      
      (test #t has-lam? #'(any lambda))
      (test #f has-lam? #'(any lambada))

      (test #t has-et-lam? #'(any lambda))
      (test #f has-et-lam? #'(any lambada))

      ;; mz: prefix is there in normal environment:
      (test #t has-mz:lam? #'(any lambda))
      (test #f has-et-mz:lam? #'(any lambda))
      (test #f has-mz:lam? #'(any mz:lambda))
      (test #t has-et-mz:lam? #'(any mz:lambda))

      ;; mtax in both places normal env:
      (test #t has-mtax? #'(any mtax))
      (test #f has-mtax? #'(any a:mtax))
      (test #f has-et-mtax? #'(any mtax))
      (test #t has-et-mtax? #'(any a:mtax))

      ;; no mtby here
      (test #f has-mtby? #'(any mtby))
      (test #t has-mtby? #'(any b:mtby))
      (test #f has-et-mtby? #'(any mtby))
      (test #f has-et-mtby? #'(any b:mtby))))

  (provide run-mt2-test))

(require mt2)
(run-mt2-test test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '(1 2 3) syntax-object->datum (syntax (1 2 3)))
(test '(1 ... 2 3) syntax-object->datum (syntax (... (1 ... 2 3))))

(syntax-test #'(syntax (a (... ...))))
(syntax-test #'(syntax (... ...)))

(report-errs)
