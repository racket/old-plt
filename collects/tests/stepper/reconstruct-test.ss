(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))
(require (prefix reconstruct: (lib "reconstruct.ss" "stepper" "private")))
(require (lib "shared.ss" "stepper" "private"))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-annotater)


(define (wrap-expand-unwrap stx language-level-spec)
  (let* ([wrapped (datum->syntax-object #f `(module test-module ,language-level-spec
                                              ,@stx))]
         [expanded (expand wrapped)])
    (with-syntax ([(module name lang (_ . exprs)) expanded])
      (syntax->list (syntax exprs)))))

(test '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5)))
      map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))

(define (make-break num-steps expr action)
  (let ([counter num-steps])
    (lambda (mark-set key break-kind returned-value-list)
      (if (> counter 0)
          (set! counter (- counter 1))
          (let ([mark-list (continuation-mark-set->list mark-set key)])
            (action (reconstruct:reconstruct-current expr mark-list break-kind returned-value-list)))))))
        
(define (annotate-expr stx lang num-steps action)
  (let loop ([env annotate:initial-env-package] [exprs (if lang 
                                                           (wrap-expand-unwrap (list stx) lang)
                                                           (list (expand stx)))])
    (if (null? exprs)
        null
        (let*-values ([(break) (make-break num-steps stx action)]
                      [(annotated new-env)
                       (annotate:annotate (car exprs) env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr exprs)))))))

(define (test-expr stx lang num-steps selector)
  (let/ec k
    (eval (selector (annotate-expr stx lang num-steps k)))))

(test `((,highlight-placeholder) (+)) test-expr #'+ 'mzscheme 0 cadr)
(test `(((,highlight-placeholder 3 4)) (+)) test-expr #'(+ 3 4) 'mzscheme 0 cadr)
(test `((,highlight-placeholder) ((+ 3 4))) test-expr #'(+ 3 4) 'mzscheme 1 cadr)

(syntax-object->datum (cadr (annotate-expr #'(+ 3 4) 'mzscheme 0 (lambda (x) x))))



(report-errs)