;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test.ss
;; Richard Cobbe
;; $Id: test.ss,v 1.21 2005/01/19 21:21:30 cobbe Exp $
;;
;; Contains SchemeUnit assertions and other things of convenience for
;; testing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module test mzscheme

  (require (lib "util.ss" "schemeunit")
           (lib "test.ss" "schemeunit"))

  (print-struct #t)

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
           hash-table->sexpr
           require/expose
           (all-from (lib "test.ss" "schemeunit"))))
