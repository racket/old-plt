(module structs mzscheme

  ;; All of the structures (and data definitions) which will need to be used
  ;; in multiple modules of the lexer.

  (provide re-ast->chars)

  (define-syntax define-struct/export
    (lambda (stx)
      (syntax-case stx ()
        ((_ n f)
	 (syntax (begin (define-struct n f) (provide (struct n f))))))))

;; Used to allow us to expand only define-lex-abbrev in an re
(define-struct/export lex-abbrev (abbrev))

;; re-ast = (make-epsilon)
;;        | (make-syms (char list | eof | marker) nat)
;;        | (make-altern re-ast re-ast)
;;        | (make-concat re-ast re-ast)
;;        | (make-kstar re-ast)
;; marker = (make-marker nat)
(define-struct/export epsilon ())
(define-struct/export syms (chars pos))
(define-struct/export altern (left right))
(define-struct/export concat (left right))
(define-struct/export kstar (rexp))
(define-struct/export marker (rule-num))

;; dfa = (make-dfa state (state list) (transition list))
;; state = (int list)
;; transition = (make-transition state ((char | eof) state) list)
(define-struct/export dfa (start states trans))
(define-struct/export transition (from to))

;; lexer-table = 
;;   (make-table (int vector) (int vector) int (int vector))
(define-struct/export table (trans eof start actions))


;; re-ast->chars : re-ast -> (char | eof | nat) vector
;; Converts n into a vector of its characters.
;; Also marks the position of each syms in the ast.
(define (re-ast->chars n)
  (letrec ((index 0)
	   (re->list
	    (lambda (n)
	      (cond
	       ((epsilon? n) null)
	       ((syms? n) 
		(set-syms-pos! n index)
		(set! index (add1 index))
		(list (syms-chars n)))
	       ((altern? n) (append (re->list (altern-left n))
				    (re->list (altern-right n))))
	       ((concat? n) (append (re->list (concat-left n))
				    (re->list (concat-right n))))
	       ((kstar? n) (append (re->list (kstar-rexp n))))))))
    (list->vector (re->list n))))

)
