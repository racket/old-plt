
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
(test 0 syntax-column (datum->syntax-object #f 10 '(aha 1 0 1 0)))

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
(err/rt-test (datum->syntax-object #f 10 '(a 11 -1 13 0)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 0 0)))

(syntax-test #'quote-syntax)
(syntax-test #'(quote-syntax))
(syntax-test #'(quote-syntax . 7))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some syntax-case patterns
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 17 'syntax-case (syntax-case '(1 1 1) () [(1 ...) 17]))

(define-syntax sd (syntax-rules () [(_ v) (syntax-object->datum (syntax v))]))

(test '(3 1 2) 'syntax-case (syntax-case '(1 2 3) () [(a ... b) (sd (b a ...))]))
(test 5 'syntax-case (syntax-case '(1 2 3) () [(a ... b . c) (sd (b a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 3 . 4) () [(a ... b . c) (sd (b a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b . c)) (sd (b a ... c))][_else 5]))
(test '((3) 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b ... . c)) (sd ((b ...) a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b ... . c)) (sd (b ... a ... c))][_else 5]))
(test '((3) 1 2 4) 'syntax-case (syntax-case '(1 2 ((3) . 4)) () [(a ... ((b ...) ... . c)) (sd ((b ...) ... a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 ((3) . 4)) () [(a ... ((b ...) ... . c)) (sd (b ... ... a ... c))][_else 5]))

(syntax-test (quote-syntax (syntax-case 0 () [(a ... b c ...) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ... b . (c ...)) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ... ...) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'a][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'((a ...) ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'(a ... ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'a][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'(a ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'(a ... ... ...)][_else 5])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test basic expansion and property propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-map f)
  (lambda (l)
    (if (pair? l)
	(cons ((tree-map f) (car l))
	      ((tree-map f) (cdr l)))
	(if (null? l)
	    null
	    (f l)))))

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
(test '(mcr) (tree-map syntax-e) (syntax-property se 'origin))

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
(test '(mcr2) (tree-map syntax-e) (syntax-property se 'origin))

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
(test '(mcr2) (tree-map syntax-e) (syntax-property se 'origin))

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

(test '(mcr2 mcr5)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-step macro chain with expansion

(define-syntax mcr7
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (local-expand (syntax x) '(internal-define) (list (quote-syntax #%datum)))])))

(define s (quote-syntax (mcr7 (mcr2 5))))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e (cadr (syntax-e s))))) syntax-e se)

(test '((mcr2) mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with one expansion

(define s (quote-syntax (mcr5 (mcr7 (mcr2 5)))))
(define se (expand-once (expand-once s)))

(test '((mcr2) mcr7 mcr5)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with other expansion

(define s (quote-syntax (mcr7 (mcr5 (mcr2 5)))))
(define se (expand-once s))

(test '((mcr2 mcr5) mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #%app, etc.

(define s (syntax-property (quote-syntax (add1 5)) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

(define s (syntax-property (quote-syntax 5) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check tracking of primitive expanders

(test '(let) (tree-map syntax-e) (syntax-property (expand #'(let ([x 10]) x)) 'origin))
(test '(let let*) (tree-map syntax-e) (syntax-property (expand #'(let* ([x 10]) x)) 'origin))
(test '(let) (tree-map syntax-e) (syntax-property (expand #'(let loop ([x 10]) x)) 'origin))
(test '(letrec) (tree-map syntax-e) (syntax-property (expand #'(letrec ([x 10]) x)) 'origin))
(test '(let*-values) (tree-map syntax-e) (syntax-property (expand #'(let*-values ([(x) 10]) x)) 'origin))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identifier-binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '(#%kernel lambda mzscheme lambda #f)  identifier-binding #'lambda)
(test '(#%more-scheme delay mzscheme delay #f)  identifier-binding #'delay)
(test '(#%kernel #%module-begin mzscheme #%plain-module-begin #f)  identifier-binding #'#%plain-module-begin)
(require (rename mzscheme #%pmb #%plain-module-begin))
(test '(#%kernel #%module-begin mzscheme #%plain-module-begin #f)  identifier-binding #'#%pmb)

(let ([b (identifier-binding (syntax-case (expand #'(module m mzscheme
						      (require (rename (lib "htdp-intermediate.ss" "lang") bcons cons))
						      bcons)) ()
			       [(mod m mz (#%mod-beg for-syntax req cons))
				(let ([s (syntax cons)])
				  (test 'bcons syntax-e s)
				  s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
	       [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"teachprims.ss" values real)
    (test 'beginner-cons cadr b)
    (test '(lib "htdp-intermediate.ss" "lang") values nominal)
    (test 'cons cadddr b)))

(let ([b (identifier-binding (syntax-case (expand #'(module m (lib "htdp-intermediate.ss" "lang")
						      cons)) ()
			       [(mod m beg (#%mod-beg cons))
				(let ([s (syntax cons)])
				  (test 'cons syntax-e s)
				  s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
	       [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"teachprims.ss" values real)
    (test 'beginner-cons cadr b)
    (test '(lib "htdp-intermediate.ss" "lang") values nominal)
    (test 'cons cadddr b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval versus eval-syntax, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless building-flat-tests?

  (test eval eval 'eval)
  (test eval eval eval)
  (test eval eval #'eval)
  (test eval eval (datum->syntax-object #f 'eval))

  (err/rt-test (eval-syntax 'eval))
  (err/rt-test (eval-syntax eval))
  (test eval eval-syntax #'eval)
  (test #t 
	'eval-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (eval-syntax (datum->syntax-object #f 'eval))))

  (test eval (current-eval) 'eval)
  (test eval (current-eval) eval)
  (test eval (current-eval) #'eval)
  (test #t 
	'current-eval-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  ((current-eval) (datum->syntax-object #f 'eval))))

  (test eval 'compile (eval (compile 'eval)))
  (test eval 'compile (eval (compile eval)))
  (test eval 'compile (eval (compile #'eval)))
  (test eval 'compile (eval (compile (datum->syntax-object #f 'eval))))

  (err/rt-test (compile-syntax 'eval))
  (err/rt-test (compile-syntax eval))
  (test eval 'compile (eval (compile-syntax #'eval)))
  (test #t 
	'compile-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (compile-syntax (datum->syntax-object #f 'eval))))

  (test eval 'expand (eval (expand 'eval)))
  (test eval 'expand (eval (expand eval)))
  (test eval 'expand (eval (expand #'eval)))
  (test eval 'expand (eval (expand (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax 'eval))
  (err/rt-test (expand-syntax eval))
  (test eval 'expand (eval (expand-syntax #'eval)))
  (test #t 
	'expand-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (expand-syntax (datum->syntax-object #f 'eval))))

  (test eval 'expand-once (eval (expand-once 'eval)))
  (test eval 'expand-once (eval (expand-once eval)))
  (test eval 'expand-once (eval (expand-once #'eval)))
  (test eval 'expand-once (eval (expand-once (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax-once 'eval))
  (err/rt-test (expand-syntax-once eval))
  (test eval 'expand-once (eval (expand-syntax-once #'eval)))
  (test #t 
	'expand-syntax-once
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (expand-syntax-once (datum->syntax-object #f 'eval))))

  (test eval 'expand-to-top-form (eval (expand-to-top-form 'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form #'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax-to-top-form 'eval))
  (err/rt-test (expand-syntax-to-top-form eval))
  (test eval 'expand-to-top-form (eval (expand-syntax-to-top-form #'eval)))
  (test #t syntax? (expand-syntax-to-top-form (datum->syntax-object #f 'eval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; origin tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks whether stx includes an mapping for
;;  a `where' form (indicated by a symbol) going back to
;;  a `what' form (another symbol)
;; If `where' is #f, look for the annotation on a let...-values
;;  binding clause
(define (has-stx-property? stx where what prop)
  (define (has-p? stx)
    (let ([p (syntax-property stx prop)])
      (and p
	   (let loop ([p p])
	     (cond
	      [(pair? p) (or (loop (car p))
			     (loop (cdr p)))]
	      [else (and (identifier? p)
			 (eq? what (syntax-e p)))])))))
  
  (let loop ([stx stx])
    (or (and (has-p? stx)
	     (or (eq? #t where)
		 (eq? (syntax-e stx) where)
		 (and (pair? (syntax-e stx))
		      (eq? (syntax-e (car (syntax-e stx)))
			   where))))
	(syntax-case stx (lambda case-lambda begin begin0
				 set! with-continuation-mark
				 if #%app module #%plain-module-begin
				 define-values)
	  [(lambda formals expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(case-lambda [formals expr ...] ...)
	   (ormap (lambda (l)
		    (ormap loop (syntax->list l)))
		  (syntax->list #'((expr ...) ...)))]
	  [(let ([(id ...) rhs] ...) expr ...)
	   (or (module-identifier=? #'let #'let-values)
	       (module-identifier=? #'let #'letrec-values))
	   (or (and (boolean? where)
		    (syntax-case stx ()
		      [(let [clause ...] expr)
		       (ormap has-p? (syntax->list #'(clause ...)))]))
	       (ormap loop (syntax->list #'(expr ...)))
	       (ormap loop (syntax->list #'(rhs ...))))]
	  [(begin expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(begin0 expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(set! id expr)
	   (loop #'expr)]
	  [(with-continuation-mark key val expr)
	   (or (loop #'key) (loop #'val) (loop #'expr))]
	  [(if test then else)
	   (or (loop #'test) (loop #'then) (loop #'else))]
	  [(#%app expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(module name init body)
	   (loop #'body)]
	  [(#%plain-module-begin expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(define-values (id ...) expr)
	   (loop #'expr)]
	  [_else #f]))))

(test #t has-stx-property? (expand #'(let ([x 1]) 2)) 'let-values 'let 'origin)

;; The define-struct macro expands to begin,
(test #t has-stx-property? (expand #'(define-struct x (a))) 'begin 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme (define-struct x (a)))) 'define-values 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme (define-struct x (a)))) 'define-syntaxes 'define-struct 'origin)

;; The s macro also expands to begin:
(test #t has-stx-property? (expand #'(module m mzscheme 
				 (define-syntax (s stx)
				   #'(begin
				       (+ 1 10)
				       14))
				 s))
      '#%app 's 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme 
				 (define-syntax (s stx)
				   #'(begin
				       (+ 1 10)
				       14))
				 (let ()
				   s)))
      '#%app 's 'origin)

;; Check per-clause origin from internal-defn conversion
(test #t has-stx-property? (expand #'(let () (define x 1) x)) #f 'define 'origin)
(test #t has-stx-property? (expand #'(let () (define-struct x (a)) 12)) #f 'define-struct 'origin)

;; Disappearing syntax decls:
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) (define y 12) 10)) 'letrec-values 'x 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-struct s (x)) 10)) 'letrec-values 's 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) 10)) 'begin 'x 'disappeared-binding)
(test #f has-stx-property? (expand #'(fluid-let-syntax ([x 1]) 10)) 'begin 'x 'disappeared-binding)

;; Disappearing use:
(test #t has-stx-property? (expand #'(let () (define-struct a (x)) (define-struct (b a) (z)) 10))
      #f 'a 'disappeared-use)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protected identifiers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ++p mzscheme 
  (define ++c 12)
  (define-syntax (++goo stx) #'++c)
  (provide ++goo))
(module ++q mzscheme 
  (require-for-syntax ++p)
  (define ++d 11) 
  (define-syntax (++o stx) #'++d)
  (define-syntax (++s stx)
    (syntax-case stx ()
      [(_ id) #'(define-syntax (id stx) 
		  (datum->syntax-object #'here (++goo)))]))
  (define-syntax (++t stx) (syntax-case stx () [(_ id) #'(define-values (id) ++d)]))
  (define-syntax (++t2 stx) #'(begin ++d))
  (define-syntax (++t3 stx) (syntax-property #'(begin0 ++d) 'certify-mode 'transparent))
  (define-syntax (++t4 stx) (syntax-case stx () [(_ id) #'(define id ++d)]))
  (define-syntax (++v stx) #'(begin0 ++d))
  (define-syntax (++v2 stx) #'(++d))
  (define-syntax (++v3 stx) (syntax-property #'(begin ++d) 'certify-mode 'opaque))
  (define-syntax ++ds 17)
  (define-syntax (++check-val stx)
    (syntax-case stx ()
      [(_ id) (datum->syntax-object #'here (add1 (syntax-local-value #'id)))]))
  (define-syntax (++o2 stx) #'(++check-val ++ds))
  (define-syntax (++apply-to-ds stx) 
    (syntax-case stx ()
      [(_ id) #'(id ++ds)]))
  (define-syntax (++apply-to-d stx) 
    (syntax-case stx ()
      [(_ id) #'(id ++d)]))
  (provide ++o ++o2 ++s ++t ++t2 ++t3 ++t4 ++v ++v2 ++v3
	   ++apply-to-d ++apply-to-ds))

(require ++q)
(++s ++ack)
(test 12 values ++ack)
(test 11 values ++v)
(test 11 values ++o)
(test 18 values ++o2)
(test 13 values (let () (++t id) 13))

(let-syntax ([goo (lambda (stx)
		    (syntax-case stx ()
		      [(_ id) (datum->syntax-object #'here (sub1 (syntax-local-value #'id)))]))])
  (test 16 'goo (++apply-to-ds goo)))

(test 11 eval-syntax (expand-syntax #'++o))

(test 11 eval-syntax (syntax-case (expand-syntax #'++t2) ()
		       [(_ x) #'x]))
(test 11 eval-syntax (syntax-case (expand #'(++t z)) ()
		       [(d-v (_) x) #'x]))
(test 11 eval-syntax (syntax-case (expand-syntax #'++t3) ()
		       [(_ x) #'x]))
(test 11 eval-syntax (syntax-case (expand #'(++t4 z)) ()
		       [(d-v (_) x) #'x]))

(err/rt-test (teval (syntax-case (expand #'++v) ()
		      [(_ x) #'x]))
	     exn:fail:syntax?)
(err/rt-test (teval (syntax-case (expand #'++v2) ()
		      [(_ x) #'x]))
	     exn:fail:syntax?)
(err/rt-test (teval (syntax-case (expand #'++v3) ()
		      [(_ x) #'x]))
	     exn:fail:syntax?)

(let ([expr (expand-syntax #'++v)])
  (test #f syntax-recertify-constrained? expr #f (current-inspector))
  (let ([ctx (syntax-extend-certificate-context expr #f)])
    (test #t syntax-recertify-constrained? expr ctx (current-inspector))
    (test expr syntax-recertify expr expr (current-inspector))
    (let ([new (syntax-recertify #'no-marks expr (current-inspector))])
      (test #t syntax? new)
      (test 'no-marks syntax-e new))
    (test #t syntax? (syntax-recertify (syntax-case expr ()
					 [(beg id) #'beg])
				       expr (current-inspector)))
    (err/rt-test (syntax-recertify (syntax-case expr ()
				     [(beg id) #'id])
				   expr (current-inspector)))
    (test #t syntax? (syntax-recertify (datum->syntax-object expr (syntax-e expr))
				       expr (current-inspector)))
    (err/rt-test (syntax-recertify (syntax-case expr ()
				     [(beg id) #'(ack id)])
				   expr (current-inspector)))))

(let ([expr (expand-syntax #'(++apply-to-d ack))])
  (test #f syntax-recertify-constrained? expr #f (current-inspector))
  (let ([ctx (syntax-extend-certificate-context expr #f)])
    (test #t syntax-recertify-constrained? expr ctx (current-inspector))
    (test '(#%app (#%top . ack) ++d) syntax-object->datum expr)
    (let ([try (lambda (cvt? other)
		 (syntax-recertify (datum->syntax-object 
				    expr
				    (cons (car (syntax-e expr))
					  ((if cvt?
					       (lambda (x) (datum->syntax-object
							    (cdr (syntax-e expr))
							    x))
					       values)
					   (cons
					    other
					    (cdr (syntax-e (cdr (syntax-e expr))))))))
				   expr
				   (current-inspector)))])
      (test #t syntax? (try #f #'other!))
      (let ([new (try #t #'other!)])
	(test #t syntax? new)
	(test '(#%app other! ++d) syntax-object->datum new))
      (err/rt-test (try #t (syntax-case expr ()
			     [(ap _ d) #'d]))))))

    
;; ----------------------------------------

(module ++m mzscheme 
  (define ++x 10) 
  (provide (protect ++x)))
(module ++n mzscheme 
  (require ++m) 
  (define ++y ++x)
  (define-syntax (++y-macro stx) #'++x)
  (define-syntax (++y-macro2 stx) (datum->syntax-object stx '++x))
  (define-syntax (++u-macro stx) #'++u)
  (define-syntax ++u2 (make-rename-transformer #'++u))
  (define ++u 8) ; unexported
  (provide ++y ++y-macro ++y-macro2 ++u-macro ++u2))
(require ++n)

(test 10 values ++y)
(test 10 values ++y-macro)
(test 8 values ++u-macro)
(test 8 values ++u2)

(require ++m)

(test 10 values ++x)
(test 10 values ++y-macro2)

(let ()
  (define n (current-namespace))
  (define n2 (make-namespace))
  (define i (make-inspector))

  (parameterize ([current-namespace n2])
    (namespace-attach-module n '++n))

  (parameterize ([current-code-inspector i]
		 [current-namespace n2])
    (teval '(require ++n))

    (test 10 teval '++y)
    (test 10 teval '++y-macro)
    (test 8 teval '++u-macro)
    (test 8 teval '++u2)

    (err/rt-test (teval '++y-macro2) exn:fail:contract:variable?)
    (err/rt-test (teval '++x) exn:fail:contract:variable?)

    (teval '(require ++m))
    (err/rt-test (teval '++x) exn:fail:syntax?)
    (err/rt-test (teval '++y-macro2) exn:fail:syntax?)
    
    (teval '(module zrt mzscheme 
	      (require ++n)
	      (define (vy) ++y)
	      (define (vy2) ++y-macro)
	      (define (vu) ++u-macro)
	      (define (vu2) ++u2)
	      (provide vy vy2 vu vu2)))
    (teval '(module zct mzscheme 
	      (require-for-syntax ++n)
	      (define-syntax (wy stx) (datum->syntax-object #'here ++y))
	      (let-syntax ([goo ++y-macro]) 10)
	      (define-syntax (wy2 stx) (datum->syntax-object #'here ++y-macro))
	      (define-syntax (wu stx) (datum->syntax-object #'here ++u-macro))
	      (provide wy wy2 wu)))

    (teval '(require zct))

    (test 10 teval 'wy)
    (test 10 teval 'wy2)
    (test 8 teval 'wu)

    (teval '(require zrt))

    (test 10 teval '(vy))
    (test 10 teval '(vy2))
    (test 8 teval '(vu))
    (test 8 teval '(vu2)))
  
  (let ([old-insp (current-code-inspector)])
    (parameterize ([current-code-inspector i]
		   [current-namespace n2])
      (namespace-unprotect-module old-insp '++m)))

  (parameterize ([current-code-inspector i]
		 [current-namespace n2])
    (test 10 teval '++y-macro)
    (test 10 teval '++y-macro2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
