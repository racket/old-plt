(module tester-structs mzscheme
  (provide
   (struct test-group (description tests))
   (struct test (description text thunk expectation))
   (struct expect (output-criterion print))
   (struct received (value print))
   (struct error (exception))
   (struct finish (value)))
  
;; test-group   ::= (make-test-group str (listof test))
;; test         ::= (make-test str sexpr (-> value) expect)
;; expect       ::= (make-struct (union (value -> bool) output-spec)
;;                               (union str #f))
;;                  [str to match printed output, #f = don't care]
;; received     ::= (make-received output-spec str)
;; output-spec  ::= error | finish
;; error        ::= (make-error str)
;; finish       ::= (make-finish value) 


(define-struct test-group (description tests))
(define-struct test (description text thunk expectation))
(define-struct expect (output-criterion print))
(define-struct received (value print))            
(define-struct error (exception))
(define-struct finish (value)))