;(plt:require-library "sparams.ss")

(define plt:mzscheme-parameters@
  (unit/sig plt:parameters^
    (import)
    (define case-sensitive? (not (eq? 'a 'A)))
    (define unmatched-cond/case-is-error?
      (with-handlers ((void (lambda (e) #t)))
        (cond)
        #f))
    (define allow-set!-on-undefined?
      (with-handlers ((void (lambda (e) #f)))
	(eval `(set! ,(gensym) 5))
	#t))
    (define allow-improper-lists? #t)
    (define check-syntax-level 'advanced)))
