(module sexp-to-re mzscheme

  ;; the parse function that builds an ast.

  (require "structs.ss"
	   (lib "list.ss"))
  (provide parse)

;; The input should be a syntax object which conforms to:
;; re = char                       match the given character
;;    | (make-marker nat)          cannot match, a placeholder used in the 
;;                                   dfa algorithm
;;    | symbol                     match the sequence of chars in the symbol
;;    | string                     match its sequence of characters
;;    | (symbol)                   expand the lex abbreviation named symbol
;;    | (* re)                     match 0 or more re
;;    | (+ re)                     match 1 or more re
;;    | (? re)                     match 0 or 1 re
;;    | (: re ...1)                match one of the listed re
;;    | (@ re ...)                 match each re in succession
;;    | (- char char)              match any character between two (inclusive)
;;    | (^ char_or_range ...1)     match any character not listed
;; (the null concatenation `(@) means epsilon)


;; make-range : int * int -> char list
;; creates a list of all chars between i and j.  i <= j
(define (make-range i j)
  (letrec ((make-range 
	    (lambda (i j)
	      (cond
	       ((= i j) (list (integer->char i)))
	       (else 
		(cons (integer->char i) (make-range (add1 i) j)))))))
    (make-range i j)))

;; parse-assoc: ('a * 'a -> 'b) * 'a list -> 'b
;; like foldr, but l must have length at least 2, and the base case is
;; combine of the last 2 elements of l
(define (parse-assoc combine l)
  (cond
   ((null? (cddr l)) (combine (car l) (cadr l)))
   (else
    (combine (car l) (parse-assoc combine (cdr l))))))

;; union : char list * char list -> char list
;; Combines 2 sorted, duplicate-free lists into 1, removing duplicates.
(define (union l1 l2)
  (cond
   ((null? l2) l1)
   ((null? l1) l2)
   (else (let ((cl1 (car l1))
	       (cl2 (car l2)))
	   (cond
	    ((> (char->integer cl1) (char->integer cl2))
	     (cons cl2 (union l1 (cdr l2))))
	    ((< (char->integer cl1) (char->integer cl2))
	     (cons cl1 (union (cdr l1) l2)))
	    (else (union (cdr l1) l2)))))))

;; group-chars: re-ast list -> re-ast list
;; Takes all the (character containing) syms in l and combines them into 1 
;; element
(define (group-chars l)
  (letrec (
	   ;; group : re-ast list * char list * re-ast list -> re-ast list
	   (group
	    (lambda (l s acc)
	      (cond
	       ((null? l) (cons (make-syms s -1) acc))
	       ((and (syms? (car l)) (list? (syms-chars (car l))))
		(group (cdr l) (union (syms-chars (car l)) s) acc))
	       (else (group (cdr l) s (cons (car l) acc)))))))
    (group l null null)))

(define (stx-error syntax message)
  (raise-syntax-error #f message syntax))

;; parse : syntax-object -> re-ast
;; checks for errors and generates the ast for s
(define (parse s)
  (let ((se (syntax-e s)))
    (cond
     ((char? se) (make-syms (list se) -1))
     ((marker? se) (make-syms se -1))
     ((or (string? se) (symbol? se))
      (let ((l (string->list 
		(if (symbol? se)
		    (symbol->string se)
		    se))))
	(cond
	 ((= 1 (length l)) (make-syms l -1))
	 (else (parse-assoc make-concat 
			    (map (lambda (x) (make-syms (list x) -1)) l))))))
     ((list? se)
      (let ((oper (syntax-e (car se)))
	    (expand (syntax-local-value (car se) (lambda () #f)))
	    (len-se (length se)))
	(cond
	 ((and (lex-abbrev? expand) (= len-se 1))
	  (parse (lex-abbrev-abbrev expand)))
	 ((and (eq? oper 'eof)) (make-syms eof -1))
	 ((and (eq? oper '*) (= 2 len-se))
	  (make-kstar (parse (cadr se))))
	 ((eq? oper '*)
	  (stx-error 
	   s 
	   "operation must have exactly one operand"))
	 ((and (eq? oper '+) (= len-se 2))
	  (let ((arg (parse (cadr se))))
	    (make-concat arg (make-kstar arg))))
	 ((eq? oper '+)
	  (stx-error 
	   s 
	   "operation must have exactly one operand"))
	 ((and (eq? oper '?) (= len-se 2))
	  (make-altern (make-epsilon) (parse (cadr se))))
	 ((eq? oper '?)
	  (stx-error 
	   s 
	   "operation must have exactly one operand"))
	 ((and (eq? oper ':) (> len-se 2))
	  (let* ((parts (group-chars (map parse (cdr se))))
		 (l (length parts)))
	    (if (= 1 l)
		(car parts)
		(parse-assoc make-altern parts))))
	 ((and (eq? oper ':) (= len-se 2))
	  (parse (cadr se)))
	 ((eq? oper ':)
	  (stx-error 
	   s 
	   "operation must have at least one operand"))
	 ((and (= len-se 1) (eq? oper '@))
	  (make-epsilon))
	 ((and (= len-se 2) (eq? oper '@))
	  (parse (cadr se)))
	 ((and (eq? oper '@))
	  (parse-assoc make-concat (map parse (cdr se))))
	 ((and (eq? oper '-) (= len-se 3))
	  (let ((c1 (parse (cadr se)))
		(c2 (parse (caddr se))))
	    (if (and (syms? c1) (syms? c2)
		     (list? (syms-chars c1)) (list? (syms-chars c2))
		     (null? (cdr (syms-chars c1))) 
		     (null? (cdr (syms-chars c2))))
		(let ((i1 (char->integer (car (syms-chars c1))))
		      (i2 (char->integer (car (syms-chars c2)))))
		  (if (<= i1 i2)
		      (make-syms (make-range i1 i2) -1)
		      (stx-error
		       s
		       (format "first argument ~a does not preceed second argument ~a" c1 c2))))
		((eq? oper '-)
		 (stx-error 
		  s 
		  "operation expects single character arguments")))))
	 ((eq? oper '-)
	  (stx-error
	   s
	   "operation  must have exactly two arguments"))
	 ((and (eq? oper '^) (> len-se 1))
	  (letrec ((res (map parse (cdr se)))
		   (v (make-vector 256 #t)))
	    (if (not (andmap (lambda (x)
			       (and (syms? x)
				    (list? (syms-chars x))))
			     res))
		(stx-error
		 s
		 "expression expects single character or range arguments"))
	    (for-each (lambda (sym)
			(for-each (lambda (char)
				    (vector-set! v (char->integer char) #f))
				  (syms-chars sym)))
		      res)
	    (make-syms
	     (let loop ((i 0))
	       (cond
		((= i 256) null)
		((vector-ref v i)
		 (cons (integer->char i) (loop (add1 i))))
		(else
		 (loop (add1 i)))))
	     -1)))
	 ((eq? oper '^)
	  (stx-error
	   s
	   "operation must have at least one argument"))
	 (else
	  (stx-error 
	   s
	   "invalid operator or abbreviation.")))))
     (else
      (stx-error
       s
       "invalid regular expression")))))

)






