(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))
(require (prefix reconstruct: (lib "reconstruct.ss" "stepper" "private")))
(require (lib "shared.ss" "stepper" "private"))
(require (lib "highlight-placeholder.ss" "stepper" "private"))
(require (lib "my-macros.ss" "stepper" "private"))
(require (lib "model-settings.ss" "stepper" "private"))
(require (lib "marks.ss" "stepper" "private"))
(require (lib "class.ss"))
(require (lib "etc.ss"))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-reconstruct)

; this following code is probably a good idea, but not right now. For now, I just want
; to get the stepper working.

;; a step-queue object collects steps that come from
;; breakpoints, and sends them to the view in nice
;; tidy little bundles
;(define step-queue%
;  (class object% ()
;    
;    (field (queue #f)) ; : (listof (list syntax symbol mark-list (listof TST)))
;    
;    ; : (syntax symbol mark-list (listof TST)) -> (void)
;    ; effects: queue
;    (define (add-step . args)
;      (set! queue (append queue (list args)))
;      (try-match))
;
;    ; ( -> (void))
;    ; effects: queue
;    ; take action based on the head of the queue
;    (define (try-match)
;      (unless (null? queue)
;        (case (cadr (car queue))
;          ((
;      

; collect-in-pairs-maker : ((list 'a 'a) -> 'b) -> (boolean 'a -> (union 'b void)) 
(define (collect-in-pairs-maker action)
  (let ([stored-first #f]
        [have-first? #f])
    (lambda (first-kind? value)
      (if first-kind?
          (begin 
            (set! stored-first value)
            (set! have-first? #t))
          (let ([temp-stored stored-first]
                [temp-have? have-first?])
            (set! stored-first #f)
            (set! have-first? #f)
            (if temp-have?
                (action (list temp-stored value))
                (void)))))))

(define t (collect-in-pairs-maker (lx _)))
(test (void) t #f 'ahe)
(test (void) t #t 13)
(test (void) t #t 'apple)
(test (list 'apple 'banana) t #f 'banana)
(test (void) t #f 'oetu)

; : ((recon-result recon-result -> (void)) box -> syntax -> break-contract)
(define (make-break action expr-box)
  (let* ([recon-call (lx (apply reconstruct:reconstruct-current _))]
         [pair-action (lambda (2-list)
                        (apply action (map recon-call 2-list)))]
         [collector (collect-in-pairs-maker pair-action)])
    (lambda (mark-set break-kind returned-value-list)
        (let ([mark-list (extract-mark-list mark-set)])
          (unless (reconstruct:skip-step? break-kind mark-list)
            (case break-kind
              ((normal-break)
               (collector #t (list (unbox expr-box) mark-list break-kind returned-value-list)))
              ((result-exp-break result-value-break)
               (collector #f (list (unbox expr-box) mark-list break-kind returned-value-list)))
              (else (error 'break "unexpected break-kind: ~a" break-kind))))))))

; : ((listof syntax?) box (recon-result recon-result -> (void)) -> (listof syntax)
(define (annotate-exprs stx-list expr-box action)
  (let* ([break (make-break action expr-box)]) 
    (let loop ([env annotate:initial-env-package] [stx-list stx-list])
      (if (null? stx-list)
          null
          (let*-values ([(annotated new-env)
                         (annotate:annotate (car stx-list) env break 'foot-wrap)])
            (cons annotated (loop new-env (cdr stx-list))))))))

; : (string -> (listof syntax)
(define (string->stx-list stx)
  (let ([port (open-input-string stx)])
    (let loop ([first-stx (read-syntax 'test-program port)])
      (if (eof-object? first-stx)
          null
          (cons first-stx (loop (read-syntax 'test-program port)))))))
  
(define (test-sequence stx expected-queue expected-completed namespace)
  (let/ec k
    (let* ([expr-box (box #f)]
           [action (lambda (before after)
                     (when (null? expected-queue)
                       (when expected-completed
                         (fprintf (current-error-port) "got an extra pair:\nbefore: ~e\nafter: ~e\n"
                                  before after)
                       (test #f (lambda (x) x) expected-completed))
                       (k (void)))
                     (test (car expected-queue) (lambda () before))
                     (test (cadr expected-queue) (lambda () after))
                     (set! expected-queue (cddr expected-queue)))])
      (parameterize ([current-namespace namespace])
        (let* ([stx-list (string->stx-list stx)]
               [expanded (map expand stx-list)]
               [annotated (annotate-exprs expanded expr-box action)]
               [eval-expr (lambda (expanded annotated)
                            (set-box! expr-box expanded)
                            (eval annotated))])
          (if expected-completed
              (test expected-completed map reconstruct:reconstruct-completed expanded (map eval-expr expanded annotated))
              (map eval-expr expanded annotated))
          (test #t null? expected-queue))))))

(define (namespace-rewrite-expr stx namespace)
  (parameterize ([current-namespace namespace])
    (map annotate:top-level-rewrite (map expand (string->stx-list stx)))))

(define mz-namespace (current-namespace))
(define (test-mz-sequence source-list result-list)
  (test-sequence source-list result-list #f mz-namespace))

(define beginner-namespace (make-namespace 'empty))
(parameterize ([current-namespace beginner-namespace])
  (namespace-attach-module mz-namespace 'mzscheme)
  (namespace-require '(lib "htdp-beginner.ss" "lang")))
(define (test-beginner-sequence source-list result-list completed-list)
  (set-fake-beginner-mode #t)
  (test-sequence source-list result-list completed-list beginner-namespace)
  (set-fake-beginner-mode #f))

(test-mz-sequence "(+ 3 4)"
                  `(((,highlight-placeholder) ((+ 3 4)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence "((lambda (x) (+ x 3)) 4)"
                  `(((,highlight-placeholder) (((lambda (x) (+ x 3)) 4)))
                    ((,highlight-placeholder) ((+ 4 3)))
                    ((,highlight-placeholder) ((+ 4 3)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence "(if 3 4 5)"
                  `(((,highlight-placeholder) ((if 3 4 5)))
                    ((,highlight-placeholder) (4))))

(test-beginner-sequence "(if (if true false true) false true)"
                        `((((if ,highlight-placeholder false true)) ((if true false true)))
                          (((if ,highlight-placeholder false true)) (false))
                          ((,highlight-placeholder) ((if false false true)))
                          ((,highlight-placeholder) (true)))
                        `(true))

(test-mz-sequence "((lambda (x) x) 3)"
                  `(((,highlight-placeholder) (((lambda (x) x) 3)))
                    ((,highlight-placeholder) (3))))

; 'begin' not yet supported by reconstruct
;(test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)"))
;                  `((((begin ,highlight-placeholder (+ 4 5))) ((+ 3 4)))
;                    (((begin ,highlight-placeholder (+ 4 5))) (7))
;                    ((,highlight-placeholder) ((begin 7 (+ 4 5))))
;                    ((,highlight-placeholder) ((+ 4 5)))
;                    ((,highlight-placeholder) ((+ 4 5)))
;                    ((,highlight-placeholder) (9))))

(test-mz-sequence "((lambda (a) (lambda (b) (+ a b))) 14)"
                  `(((,highlight-placeholder) (((lambda (a) (lambda (b) (+ a b))) 14)))
                    ((,highlight-placeholder) ((lambda (b) (+ 14 b))))))

(test-mz-sequence "((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)"
                  `(((,highlight-placeholder) (((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)))
                    ((,highlight-placeholder) ((+ 5 6)))
                    ((,highlight-placeholder) ((+ 5 6)))
                    ((,highlight-placeholder) (11))))

; reconstruct does not handle one-armed if's:
;(test-mz-sequence "(if 3 4)"
;                  `(((,highlight-placeholder) ((if 3 4)))
;                    ((,highlight-placeholder) (4))))

; reconstruct does not handle begin0

;(test-mz-sequence "(let ([a 3]) 4)"
;                  `(((,highlight-placeholder) ((let-values ([(a) 3]) 4)) (,highlight-placeholder ,highlight-placeholder) ((define-values (a_0) 3) (begin 4)))
;                    (((define a_0 3))))) 
;
;(test-mz-sequence "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
;                  `(((,highlight-placeholder) ((let-values ([(a) (+ 4 5)] [(b) (+ 9 20)]) (+ a b))) 
;                     (,highlight-placeholder ,highlight-placeholder ,highlight-placeholder) 
;                     ((define-values (a_0) (+ 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))))
;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) ((+ 4 5)))
;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (9))
;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) ((+ 9 20)))
;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) (29))
;                    (((define a_0 9) (define b_1 29)))
;                    (((+ ,highlight-placeholder b_1)) (a_0))
;                    (((+ ,highlight-placeholder b_1)) (9))
;                    (((+ 9 ,highlight-placeholder)) (b_1))
;                    (((+ 9 ,highlight-placeholder)) (29))
;                    ((,highlight-placeholder) ((+ 9 29)))
;                    ((,highlight-placeholder) (38))))

;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
;                  `((((,highlight-placeholder (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation)))
;                    (((,highlight-placeholder (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
;                    ((((lambda args ...) ,highlight-placeholder)) ((call-with-current-continuation call-with-current-continuation)))
;                    ((((lambda args ...) ,highlight-placeholder)) ((lambda args ...)))))

;(test-mz-sequence '(begin (define g 3) g)
;                  `(((,highlight-placeholder) (g))
;                    ((,highlight-placeholder) 3)))

;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))

(test-beginner-sequence "(define a (+ 3 4))"
                        `((((define a ,highlight-placeholder)) ((+ 3 4)))
                          (((define a ,highlight-placeholder)) (7)))
                        `((define a 7)))

(test-beginner-sequence "(+ 4 129)"
                        `(((,highlight-placeholder) ((+ 4 129)))
                          ((,highlight-placeholder) (133)))
                        `(133))
                        
(test-beginner-sequence "(if true 3 4)"
                        `(((,highlight-placeholder) ((if true 3 4)))
                          ((,highlight-placeholder) (3)))
                        `(3))

(test-beginner-sequence "(cond [false 4] [false 5] [true 3])"
               `(((,highlight-placeholder) ((cond (false 4) (false 5) (true 3))))
                 ((,highlight-placeholder) ((cond (false 5) (true 3))))
                 ((,highlight-placeholder) ((cond (false 5) (true 3))))
                 ((,highlight-placeholder) ((cond (true 3))))
                 ((,highlight-placeholder) ((cond (true 3))))
                 ((,highlight-placeholder) (3)))
               `(3))

(test-beginner-sequence "(cond [false 4] [else 9])"
               `(((,highlight-placeholder) ((cond [false 4] [else 9])))
                 ((,highlight-placeholder) (9)))
               `(9))

(test-beginner-sequence "(or false true false)"
                        `(((,highlight-placeholder) ((or false true false)))
                          ((,highlight-placeholder) ((or true false)))
                          ((,highlight-placeholder) ((or true false)))
                          ((,highlight-placeholder) (true)))
                        `(true))

(test-beginner-sequence "(and true false true)"
                        `(((,highlight-placeholder) ((and true false true)))
                          ((,highlight-placeholder) ((and false true)))
                          ((,highlight-placeholder) ((and false true)))
                          ((,highlight-placeholder) (false)))
                        `(false))

(test-beginner-sequence "(define (a2 x) x) (a2 4)"
                        `(((,highlight-placeholder) ((a2 4)))
                          ((,highlight-placeholder) (4)))
                        `((define (a2 x) x) 4))

(test-beginner-sequence "(define (a3 x) (if true x x)) (a3 false)"
                        `(((,highlight-placeholder) ((a3 false)))
                          ((,highlight-placeholder) ((if true false false)))
                          ((,highlight-placeholder) ((if true false false)))
                          ((,highlight-placeholder) (false)))
                        `((define (a3 x) (if true x x)) false))

(test-beginner-sequence "(define (b2 x) (and true x)) (b2 false)"
                        `(((,highlight-placeholder) ((b2 false)))
                          ((,highlight-placeholder) ((and true false)))
                          ((,highlight-placeholder) ((and true false)))
                          ((,highlight-placeholder) (false)))
                        `((define (b2 x) (and true x)) false))

(test-beginner-sequence "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                        `(((,highlight-placeholder) ((b1 false)))
                          ((,highlight-placeholder) ((and a1 true false)))
                          (((and ,highlight-placeholder true false)) (a1))
                          (((and ,highlight-placeholder true false)) (true))
                          ((,highlight-placeholder) ((and true true false)))
                          ((,highlight-placeholder) ((and true false)))
                          ((,highlight-placeholder) ((and true false)))
                          ((,highlight-placeholder) (false)))
                        `((define a1 true) (define (b1 x) (and a1 true x)) false))


(test-beginner-sequence "(define a4 +) a4"
                        `(((,highlight-placeholder) (a4))
                          ((,highlight-placeholder) (+)))
                        `((define a4 +) +))

(test-beginner-sequence "(define (f123 x) (+ x 13)) f123"
                        `()
                        `((define (f123 x) (+ x 13)) f123))

(test-beginner-sequence "(define (b x) (+ x 13)) (b 9)"
                        `(((,highlight-placeholder) ((b 9)))
                          ((,highlight-placeholder) ((+ 9 13)))
                          ((,highlight-placeholder) ((+ 9 13)))
                          ((,highlight-placeholder) (22)))
                        `((define (b x) (+ x 13)) 22))

(test-beginner-sequence "(define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))"
                        `(((,highlight-placeholder) ((mamba-rhythm (make-mamba 24 2))))
                          ((,highlight-placeholder) (24)))
                        `((define-struct mamba (rhythm tempo)) 24))

(report-errs)
