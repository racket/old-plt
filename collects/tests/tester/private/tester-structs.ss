(define-syntax (struct-module stx)
  (syntax-case stx (struct)
      [(_ mod-name (struct s-name (field ...)) ...)
       (syntax
        (module mod-name mzscheme
          (provide
           (struct s-name (field ...)) ...)
          
          (define-struct s-name (field ...)) ...))]))

(struct-module tester-structs
               (struct test-group (description initializer tests))
               (struct test (description text thunk expectation))
               (struct expect (output-criterion print))
               (struct received (value print))
               (struct error (exception))
               (struct finish (value)))

;; test-group   ::= (make-test-group str (-> void) (listof test))
;; test         ::= (make-test str sexpr (-> value) expect)
;; expect       ::= (make-struct (union (value -> bool) output-spec)
;;                               (union str #f))
;;                  [str to match printed output, #f = don't care]
;; received     ::= (make-received output-spec str)
;; output-spec  ::= error | finish
;; error        ::= (make-error str)
;; finish       ::= (make-finish value) 

