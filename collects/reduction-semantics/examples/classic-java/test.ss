;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test.ss
;; Richard Cobbe
;; $Id: test.ss,v 1.1 2005/02/02 15:06:48 cobbe Exp $
;;
;; Contains SchemeUnit assertions and other things of convenience for
;; testing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module test mzscheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))
  ;; need test.ss for assert-true.
  
  ;; An assertion for expressions that handle multiple values.
  ;;    same? : predicate for comparing expected with actual results
  ;;    expr  : expression to test
  ;;    exp1 exps ... : expected results
  (define-syntax mv-assert
    (syntax-rules ()
      [(_ same? expr exp1 exps ...)
       (call-with-values
        (lambda () expr)
        (lambda actual
          (assert-true (andmap same? actual (list exp1 exps ...)))))]))

  (define hash-table->sexpr
    (lambda (ht)
      (hash-table-map ht list)))

  (provide mv-assert
           hash-table->sexpr))
