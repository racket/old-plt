
(plt:require-library "sparams.ss")

(define plt:baby-parameters@
  (unit/sig plt:parameters^
    (import)
    (define allow-one-armed-if? #f)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define allow-improper-lists-in-lambda? #f)
    (define unmatched-cond/case-is-error? #t)
    (define check-syntax-level 'baby)))

(define plt:adult-parameters@
  (unit/sig plt:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #f)
    (define allow-improper-lists? #f)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error? #t)
    (define check-syntax-level 'adult)))

(define plt:our-parameters@
  (unit/sig plt:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? #t)
    (define allow-set!-on-undefined? #f)
    (define allow-internal-defines? #t)
    (define allow-improper-lists? #t)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error? #t)
    (define check-syntax-level 'our)))

(define plt:mzscheme-parameters@
  (unit/sig plt:parameters^
    (import)
    (define allow-one-armed-if? #t)
    (define case-sensitive? (not (eq? 'a 'A)))
    (define allow-set!-on-undefined?
      (with-handlers ((void (lambda (e) #f)))
	(eval `(set! ,(gensym) 5))
	#t))
    (define allow-internal-defines? #t)
    (define allow-improper-lists? #t)
    (define allow-improper-lists-in-lambda? #t)
    (define unmatched-cond/case-is-error?
      (with-handlers ((void (lambda (e) #t)))
	(cond)
	#f))
    (define check-syntax-level 'none)))
