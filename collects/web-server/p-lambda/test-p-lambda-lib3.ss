(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift normalize-term))

(define (normalize-def a-def)
  (syntax-case a-def (define lambda case-lambda)
    [(define f (lambda formals body-exprs ...))
     (let-values ([(new-body body-defs)
                   (normalize-term #'(begin body-exprs ...))])
       (cons
        #`(define f (lambda formals #,new-body))
        body-defs))]
    [(define f
       (case-lambda cases ...))
     (let-values ([(new-cases case-defs)
                   (normalize-cases (syntax->list #'(cases ...)))])
       (cons #`(define f
                 (case-lambda #,@new-cases))
             case-defs))]))

(define (normalize-cases cases)
  (if (null? cases) (values '() '())
      (with-syntax ([(formals body-exprs ...) (car cases)])
        (let-values ([(new-body body-defs)
                      (normalize-term #'(begin body-exprs ...))]
                     [(rest-cases case-defs) (normalize-cases (cdr cases))])
          (values
           (cons #`(formals #,new-body) rest-cases)
           (append body-defs case-defs))))))


(define myprint void)

(define (p-eval expr)
  (let/cc abort
    (parameterize [(current-abort abort)]
      (let flatten-begins ([e expr])
        (let ([top-e (expand-to-top-form e)])
          (syntax-case top-e (begin)
            [(begin exprs ...)
             (foldl
              (lambda (e old-val)
                (flatten-begins e))
              (void)
              (syntax->list #'(exprs ...)))]
            [not-a-begin
             (let ([ex (expand top-e)])
               (let-values ([(body defs) (lift ex)])
                 (let ([defs
                        (let loop ([defs defs])
                          (if (null? defs) '()
                              (append (normalize-def (car defs))
                                      (loop (cdr defs)))))])
                   (let-values ([(body body-defs) (normalize-term body)])
                     (let ([defs (append body-defs defs)])
                       (myprint "defs = ~s~n" defs)
                       (myprint "body = ~s~n" body)
                       (for-each eval defs)
                       (eval body))))))]))))))

;; ****************************************
;; adapted some material from Teach Yourself Scheme in Fixnum Days
;; for these examples.

(define-values (getit setit!)
  (let ([x 0])
    (values
     (lambda () x)
     (lambda (new-x)
       (set! x new-x)))))

(define myloop
  (p-eval (syntax (lambda (proc lis)
                    (unless (null? lis)
                      (proc (car lis))
                      (myloop proc (cdr lis)))))))

(= -1 (p-eval (syntax (let/cc exit
                        (myloop
                         (lambda (n)
                           (when (negative? n)
                             (exit n)))
                         '(12 32 45 64 -1 23 45 6))))))

(and (= -2 (p-eval (syntax (let/cc exit
                             (myloop
                              (lambda (n)
                                (setit! n)
                                (when (negative? n)
                                  (exit n)))
                              '(12 32 45 66 -2 233 456))))))
     (= -2 (getit)))

(= 4 (p-eval (syntax (+ 1 (call/cc
                           (lambda (k)
                             (+ 2 (k 3))))))))

(= 4 (p-eval (syntax (+ 1 (call/cc
                           (lambda (k)
                             (setit! k)
                             (+ 2 (k 3))))))))

;; not top level abort for this one:
;(let ([k (getit)])
;  (k 3))

(define list-product
  (p-eval (syntax (lambda (s)
                    (call/cc
                     (lambda (exit)
                       (let recur ([s s])
                         (cond
                           [(null? s) 1]
                           [else
                            (setit! (car s))
                            (if (= (car s) 0)
                                (exit 0)
                                (* (car s)
                                   (recur (cdr s))))]))))))))

(= 6 (p-eval (syntax (list-product '(1 2 3)))))
(zero? (p-eval (syntax (list-product '(0 1 2 3)))))
(zero? (p-eval (syntax (getit))))
(zero? (p-eval (syntax (list-product '(1 2 3 0 4 5 6)))))
(zero? (p-eval (syntax (getit))))

;; tree->generator: tree -> (-> number)
;; the resulting generator produces the leaves in order each time
;; it is called.
(define tree->generator
  (p-eval (syntax (lambda (tree)
                    (let ([caller (box '*)]
                          [generate-leaves (box #f)])
                      (set-box!
                       generate-leaves
                       (lambda ()
                         (let loop ([tree tree])
                           (cond
                             [(null? tree) 'skip]
                             [(pair? tree)
                              (loop (car tree))
                              (loop (cdr tree))]
                             [else
                              (call/cc
                               (lambda (rest-of-tree)
                                 (set-box! generate-leaves
                                           (lambda ()
                                             (rest-of-tree 'resume)))
                                 ((unbox caller) tree)))]))
                         ((unbox caller) '())))
                      (lambda ()
                        (call/cc
                         (lambda (k)
                           (set-box! caller k)
                           ((unbox generate-leaves))))))))))

;; same-fringe?: tree tree -> boolean
;; determine if the trees have the same leaves in the same order
(define same-fringe?
  (p-eval (syntax (lambda (tree1 tree2)
                    (let ([gen1 (tree->generator tree1)]
                          [gen2 (tree->generator tree2)])
                      (let loop ()
                        (let ([leaf1 (gen1)]
                              [leaf2 (gen2)])
                          (and (eqv? leaf1 leaf2)
                               (or (null? leaf1)
                                   (loop))))))))))

(p-eval (syntax (same-fringe? '() '())))
(p-eval (syntax (same-fringe? '(1 . 2) '(1 . 2))))
(p-eval (syntax (same-fringe? '(1 . (1 . 2)) '(1 . (1 . 2)))))
(p-eval (syntax (same-fringe? '((1 . 2) . 3) '(1 . (2 . 3)))))
(p-eval (syntax (not (same-fringe? '((3 . 2) . 1) '(1 . (2 . 3))))))
(p-eval (syntax (not (same-fringe? '((1 . 2) . 3) '(1 . (2 . 4))))))

(define-syntax (coroutine stx)
  (syntax-case stx ()
    [(_ x . body)
     (with-syntax ([resume (datum->syntax-object stx 'resume)])
       #`(let ([resume (box #f)])
           (let ([local-control-state
                  (box (lambda (x) . body))])
             (set-box! resume
                       (lambda (c v)
                         (call/cc
                          (lambda (k)
                            (set-box! local-control-state k)
                            (c v)))))
             (lambda (v)
               ((unbox local-control-state) v)))))]))

(define make-matcher-coroutine
  (p-eval (syntax
           (lambda (tree-cor-1 tree-cor-2)
             (coroutine dont-need-an-init-arg
                        (let loop ()
                          (let ([leaf1 ((unbox resume) tree-cor-1 'get-a-leaf)]
                                [leaf2 ((unbox resume) tree-cor-2 'get-a-leaf)])
                            (and (eqv? leaf1 leaf2)
                                 (or (null? leaf1) (loop))))))))))

(define make-leaf-gen-coroutine
  (p-eval (syntax (lambda (tree matcher-cor)
                    (coroutine dont-need-an-init-arg
                               (let loop ([tree tree])
                                 (cond
                                   [(null? tree) 'skip]
                                   [(pair? tree)
                                    (loop (car tree))
                                    (loop (cdr tree))]
                                   [else
                                    ((unbox resume) matcher-cor tree)]))
                               ((unbox resume) matcher-cor '()))))))

(define same-fringe2?
  (p-eval (syntax (lambda (tree1 tree2)
                    (let ([tree-cor-1 (box #f)]
                          [tree-cor-2 (box #f)]
                          [matcher-cor (box #f)])
                      (set-box! tree-cor-1
                                (make-leaf-gen-coroutine
                                 tree1
                                 (lambda (v) ((unbox matcher-cor) v))))
                      (set-box! tree-cor-2
                                (make-leaf-gen-coroutine
                                 tree2
                                 (lambda (v) ((unbox matcher-cor) v))))
                      (set-box! matcher-cor
                                (make-matcher-coroutine
                                 (lambda (v) ((unbox tree-cor-1) v))
                                 (lambda (v) ((unbox tree-cor-2) v))))
                      ((unbox matcher-cor) 'start-ball-rolling))))))

(p-eval (syntax (same-fringe2? '() '())))
(p-eval (syntax (same-fringe2? '(1 . 2) '(1 . 2))))
(p-eval (syntax (same-fringe2? '(1 . (1 . 2)) '(1 . (1 . 2)))))
(p-eval (syntax (same-fringe2? '((1 . 2) . 3) '(1 . (2 . 3)))))
(p-eval (syntax (not (same-fringe2? '((3 . 2) . 1) '(1 . (2 . 3))))))
(p-eval (syntax (not (same-fringe2? '((1 . 2) . 3) '(1 . (2 . 4))))))