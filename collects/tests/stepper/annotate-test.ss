(current-library-collection-paths '("/Users/clements/hot/plt/collects"))

(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))

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
  (list (list #'(let ([a 9] [b 12]) (lambda (b c) (+ b c) (+ a b 4))) 'mzscheme
              (lambda (stx)
                (kernel:kernel-syntax-case stx #f
                   [(let*-values _c
                       (with-continuation-mark _d _e
                                               (begin
                                                 . rest
;                                                 (break-proc_1)
;                                                 (begin
;                                                   _f ; set!
;                                                   _g ; set!
;                                                   (begin
;                                                     (break-proc_2 . break-proc_2-args)
;                                                     (with-continuation-mark
;                                                      debug-key
;                                                      (lambda ()
;                                                        (make-full-mark-proc
;                                                         source
;                                                         label-num
;                                                         .
;                                                         mark-bindings))
;                                                      (closure-capturing-proc
;                                                       (lambda (b c) _h _i)
;                                                       closure-info))))
                                                 ))
                      )
                    (syntax beg)]
                   [else (error 'test-cases "nope, didn't match")])))))
              ; do example with lifted-name arg and inferred-name storage

(define beg
  ((caddar test-cases) (cadr (annotate-expr (caar test-cases) (cadar test-cases)))))

  
  
  
  
  
