(module re-to-dfa mzscheme

  ;; Convert an re-ast into a dfa using the algorithm in section 3.9 of the
  ;; Dragon book, with the small modification that a character set
  ;; represented by the list in a syms structure gets a single position
  ;; and is not expanded into an alternation.

  (require (lib "list.ss")
	   "structs.ss"
	   "ht.ss")

  (provide re-ast->dfa)

;; union : state * state -> state
;; Combines 2 sorted, duplicate-free lists into 1, removing duplicates.
(define (union l1 l2)
  (cond
   ((null? l2) l1)
   ((null? l1) l2)
   (else (let ((cl1 (car l1))
	       (cl2 (car l2)))
	   (cond
	    ((> cl1 cl2) (cons cl2 (union l1 (cdr l2))))
	    ((< cl1 cl2) (cons cl1 (union (cdr l1) l2)))
	    (else (union (cdr l1) l2)))))))

;; traverse-re-ast : 
;;   'a * (sym -> 'a) * (re-ast * 'a * 'a -> 'a) * (re-ast * 'a * 'a -> 'a) * (re-ast * 'a -> 'a) -> 
;;     re-ast -> (re-ast 'a hash-table)
;; Generic re-ast traversal, computing a value for each node and storing it in
;; a hash table.
(define (traverse-re-ast e s a c k)
  (lambda (n)
    (letrec ((table (make-hash-table))
	     (traverse
	      (lambda (n)
		(let ((result (cond 
			       ((epsilon? n) e)
			       ((syms? n) (s n))
			       ((altern? n) (a n 
					       (traverse (altern-left n))
					       (traverse (altern-right n))))
			       ((concat? n) (c n
					       (traverse (concat-left n))
					       (traverse (concat-right n))))
			       (else (k n (traverse (kstar-rexp n)))))))
		  (hash-table-put! table n result)
		  result))))
      (traverse n)
      table)))


;; nullable : re-ast -> (re-ast bool hash-table)
;; Computes for each subexpression of n if it is the root of a subexpression 
;; whose language contains the empty string
(define nullable (traverse-re-ast #t 
				  (lambda (n) #f) 
				  (lambda (n x y) (or x y))
				  (lambda (n x y) (and x y))
				  (lambda (n x) #t)))

;; firstpos : 
;;   (re-ast bool hash-table) -> re-ast -> (re-ast state hash-table)
;; Computes the set of positions in the re that can match the first character of
;; a string matched by each subtree of a re
(define (firstpos null-table)
  (traverse-re-ast null 
		   (lambda (n) (list (syms-pos n)))
		   (lambda (n x y) (union x y))
		   (lambda (n c1 c2)
		     (if (hash-table-get null-table (concat-left n))
			 (union c1 c2)
			 c1))
		   (lambda (n x) x)))

;; lastpos : 
;;   (re-ast bool hash-table) -> re-ast -> (re-ast state hash-table)
;; Computes the set of positions in the re that can match the last character of
;; a string matched by each subtree of a re
(define (lastpos null-table)
  (traverse-re-ast null
		   (lambda (n) (list (syms-pos n)))
		   (lambda (n x y) (union x y))
		   (lambda (n c1 c2)
		     (if (hash-table-get null-table (concat-right n))
			 (union c2 c1)
			 c2))
		   (lambda (n x) x)))



;; followpos : 
;;   (re-ast state hash-table) * (re-ast state hash-table) -> 
;;     re-ast * int -> state vector
;; Returns the set of positions j s.t. there is an input ...cd... s.t. i 
;; corresponds to c and j to d
(define (followpos firstpos-table lastpos-table)
  (lambda (n num-pos)
    (letrec ((table (make-vector num-pos null))
	     
	     ;; add!: re-ast * re-ast ->
	     ;; add! updates the followpos table according to the algorithm
	     ;; at a particular node with childern c1 and c2 (which may be the
	     ;; same)
	     (add!
	      (lambda (c1 c2)
		(let ((i (hash-table-get lastpos-table c1))
		      (j (hash-table-get firstpos-table c2)))
		  (for-each (lambda (i)
			      (vector-set! 
			       table 
			       i
			       (union j (vector-ref table i))))
			    i))))

	     ;; followpos!: re-ast ->
	     ;; followpos traverses the tree computing the followpos table
	     (followpos!
	      (lambda (n)
		(cond
		 ((concat? n)
		  (let ((c1 (concat-left n))
			(c2 (concat-right n)))
		    (add! c1 c2)
		    (followpos! c1)
		    (followpos! c2)))
		 ((kstar? n)
		  (let ((c (kstar-rexp n)))
		    (add! c c)
		    (followpos! c)))
		 ((altern? n)
		  (followpos! (altern-left n))
		  (followpos! (altern-right n)))))))
      (followpos! n)
      table)))

;; re-ast->dfa : re-ast * (char list | eof | nat) vector -> dfa
;; Converts n into a dfa. chars is an inorder vector of just the 
;; symbols in the re.
(define (re-ast->dfa n chars)
  (letrec (
	   ;; get-transitions: state * state vector -> 
	   ;;   (list (char | eof) state) list * state list
	   ;; Given state (a state in the dfa) and follow 
	   ;; (the vector computed by followpos), returns a list of
	   ;; transitions out of state (each transition is a pair of
	   ;; char and a state) and a list of newly seen states
	   (get-transitions
	    (lambda (state follow)
	      (let (
		    ;; maps a character to a list of all positions
		    ;; the input state can get to on that character.
		    (table (make-hash-table))
		    
		    ;; a list of the states seen for the first time in
		    ;; this call
		    (new-states null))
		;; compute the successor state for each character
		;; by iterating over the positions in state and
		;; keeping track of which state each character at that
		;; position goes to.
		(for-each 
		 (lambda (pos)
		   (let ((char (vector-ref chars pos)))
		     (map (lambda (c)
			    (let ((already (hash-table-get
					    table
					    c
					    (lambda () null))))
			      (hash-table-put! table 
					       c
					       (union (vector-ref follow pos)
						      already))))
			  (if (list? char) char (list char)))))
		 state)
		;; manipulate the data to get the transition out of table and
		;; into the specified return format, while creating a 
		;; cannonical example of each state
		(values
		 (filter (lambda (x) (pair? (cadr x)))
			 (hash-table-map 
			  table 
			  (lambda (key value)
			    (let ((v (ht-get state-table 
					      value
					      (lambda ()
						(set! new-states
						      (cons
						       value new-states))
						(ht-put!
						 state-table
						 value
						 value)
						value))))
			      (list key v)))))
		 new-states))))

	   ;; keeps a canonical instance of each state.
	   (state-table (make-ht))
	   (nullable-table (nullable n))
	   (firstpos-table ((firstpos nullable-table) n))
	   (lastpos-table ((lastpos nullable-table) n))
	   (followpos-table 
	    ((followpos firstpos-table lastpos-table) n (vector-length chars)))
	   (first-state (hash-table-get firstpos-table n)))
    ;; We don't want to ever consider null a new state
    (ht-put! state-table null null)
    (let loop ((dstates-unmarked (list first-state))
	       (dtrans null)
	       (dstates-marked null))
      (cond
       ((null? dstates-unmarked)
	(make-dfa first-state
		  dstates-marked
		  dtrans))
       (else
	(let ((T (car dstates-unmarked)))
	  (let-values (((transitions new-states)
			(get-transitions T followpos-table)))
	    (loop (append new-states (cdr dstates-unmarked))
		  (cons (make-transition T transitions) dtrans)
		  (cons T dstates-marked)))))))))
)

