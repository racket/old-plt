(current-library-collection-paths '("/Users/clements/hot/plt/collects"))

(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))

(define (wrap-expand-unwrap stx language-level-spec)
  (let* ([wrapped (datum->syntax-object #f `(module test-module ,language-level-spec
                                              ,@stx))]
         [expanded (expand wrapped)])
    (with-syntax ([(module name lang (_ . exprs)) expanded])
      (syntax->list (syntax exprs)))))

(equal? (map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))
        '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5))))
         
(define (break) 3)

(define (annotate-expr stx lang)
  (let loop ([env annotate:initial-env-package] [exprs (wrap-expand-unwrap (list stx) lang)])
    (if (null? exprs)
        null
        (let*-values ([(annotated new-env)
                       (annotate:annotate #f (car exprs) env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr exprs)))))))

; test notes to myself:
; the never-undefined property can only be tested in the non-foot-wrap modes
; hard to test 3D values like closure-capturing-proc

(define test-cases
  ; lambda 
  (list (list #'(let ([a 9] [b 12]) (lambda (b c) (+ b c) (+ a b 4))) 'mzscheme)))

              
;              (kernel:kernel-syntax-case stx #f
;                [(with-continuation-mark
;                  _a
;                  _b
;                  (let-values _c
;                    (with-continuation-mark _d _e
;                      (begin
;                        _f ; set!
;                        _g ; set!
;                        (with-continuation-mark
;                         debug-key
;                         debug-mark
;                         (closure-capturing-proc
;                          (lambda (b c) _a _b)
;                          closure-info))))))
;                 #f])
              ; do example with lifted-name arg and inferred-name storage

(syntax-object->datum (annotate-expr (caar test-cases) (cadar test-cases)))

  
  
  
  
  
