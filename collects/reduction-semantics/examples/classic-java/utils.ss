;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; utils.ss
;; Richard Cobbe
;; July 2004
;;
;; $Id$
;;
;; General utilities used in the implementation of ClassicJava.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module utils mzscheme

  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "contract.ss"))

  ;; Ensures that we have a sufficiently powerful inspector to look at any
  ;; structures defined within an invocation of this macro.
  ;; You can use this macro in any context where a sequence of definitions is
  ;; valid.
  ;; USAGE:  (with-public-inspector DEFN ...)
  ;;   where DEFN is a definition.
  ;; The intent is that DEFN be a DEFINE-STRUCT form (or a macro which expands
  ;; to same), but I believe that value definitions should work there as well.
  (define-syntax with-public-inspector
    (lambda (stx)
      (syntax-case stx ()
        [(kw defns ...)
         (with-syntax ([(old-inspector)
                        (generate-temporaries #'(old-inspector))])
           #'(begin
               (define old-inspector (current-inspector))
               (current-inspector (make-inspector))
               defns ...
               (current-inspector old-inspector)))])))

  (provide with-public-inspector)

  (provide/contract [small-step (-> (listof red?) any? any)]
                    [big-step (-> (listof red?) any? any)]
                    [small-step-sequence (-> (listof red?) any? list?)])

  ;; small-step :: (Listof Reduction) Term -> (Union #f Term)
  ;; reduces term a single step; returns #f if no reduction possible.
  ;; signals error if multiple reductions possible.
  (define small-step
    (lambda (reductions term)
      (let ([results (reduce reductions term)])
        (cond
          [(null? results) #f]
          [(null? (cdr results)) (car results)]
          [else (error 'small-step
                       "reduction from ~a not unique: ~a" term results)]))))

  ;; big-step :: (Listof Reduction) Term -> Term
  ;; reduces term as many steps as possible and returns final result.
  ;; If at any point, multiple reductions are possible, signals an error.
  (define big-step
    (lambda (reductions term)
      (let loop ([term term])
        (cond
          [(small-step reductions term) => loop]
          [else term]))))

  ;; small-step-sequence :: (Listof Reduction) Term -> (Listof Term)
  ;; like big-step, but returns a list containing all intermediate results.
  (define small-step-sequence
    (lambda (reductions term)
      (let loop ([term term])
        (let ([next (small-step reductions term)])
          (if next
              (cons term (loop next))
              (list term))))))

  ;; produces a contract that recognizes a non-empty list of elements
  ;; which satisfy the contract c.
  (define nelistof
    (lambda (c)
      (and/c (listof c) (not/f null?))))

  ;; contract that recognizes arbitrary s-expressions.
  (define sexp?
    (flat-rec-contract sexp
                       (cons/p sexp sexp)
                       null?
                       number?
                       symbol?
                       string?
                       boolean?
                       char?))

  ;; contract that recognizes unary predicates
  (define predicate? (any? . -> . boolean?))

  (define-syntax eta
    (syntax-rules ()
      [(_ pred) (lambda (x) (pred x))]))

  (provide/contract (sexp? contract?)
                    (predicate? contract?)
                    (nelistof (-> (union contract? predicate?) contract?)))

  (provide eta))
