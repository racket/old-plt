;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; classic-java.ss
;; Richard Cobbe
;; $Id$
;;
;; Front-end to make it easy to run CJ programs in any of three different
;; styles.
;;
;; Exports three different functions:
;;
;;   * cj-trace : (-> sexp? any)
;;       runs the supplied program and displays a GUI window containing a trace
;;       of the execution.
;;
;;   * cj-eval : (-> sexp? sexp?)
;;       runs the supplied program and returns its result (as a reduction
;;       sexpr).
;;
;;   * cj-step : (-> sexp? (listof sexp?))
;;       runs the supplied program and returns a list of reduction sexprs that
;;       includes all the intermediate values with the final result at the end.
;;
;; If ever reduction is nondeterministic, cj-eval and cj-step will signal
;; errors; use cj-trace to find the branch point(s).
;;
;; All three functions expect their argument to be a complete ClassicJava
;; program according to the syntax described in grammar.txt.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module classic-java mzscheme

  (require (lib "gui.ss" "reduction-semantics")
           (lib "pretty.ss")
           "store.ss"
           "elaboration.ss"
           "parser.ss"
           "reduction.ss"
           "ast.ss"
           "utils.ss")

  (provide cj-trace cj-eval cj-step)

  (define cj-trace
    (lambda (program)
      (let ([program (elab-program (parse-program program))])
        (traces cj-lang
                cj-reductions
                (list program empty-store
                      (texpr->rexpr (program-main program)))
                (lambda (term port width text%)
                  (parameterize ([pretty-print-columns width])
                    (pretty-print (cdr term) port)))))))
  ;; We provide our own pretty printer that does not print out the program part
  ;; of the configurations.  This part doesn't change across the reductions, so
  ;; it just clutters up the screen unnecessarily.

  (define cj-eval
    (lambda (program)
      (let ([program (elab-program (parse-program program))])
        (big-step cj-reductions
                  (list program empty-store
                        (texpr->rexpr (program-main program)))))))

  (define cj-step
    (lambda (program)
      (let ([program (elab-program (parse-program program))])
        (small-step-sequence cj-reductions
                             (list program
                                   empty-store
                                   (texpr->rexpr (program-main program))))))))
