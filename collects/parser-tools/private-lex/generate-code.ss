(module generate-code mzscheme

  ;; creates the lexer

  (require "structs.ss"
	   "re-to-dfa.ss"
	   "sexp-to-re.ss"
	   (lib "list.ss"))

  (provide generate-table)

;; generate-table : syntax-object -> lexer-table
;; Creates the lexer's tables from a list of sexp-regex, action pairs.
(define (generate-table rules s)
  (let* (
	 ;; A counter
	 (index -1)

	 (regs (map (lambda (x) (car (syntax-e x))) (syntax-e rules)))

	 (actions (list->vector 
		   (map 
		    (lambda (x) 
		      (let ((action (cadr (syntax-e x))))
			(datum->syntax-object
			 action
			 `(lambda (get-start-pos get-end-pos get-lexeme lex-buf)
			    ,action)
			 action)))
		    (syntax-e rules))))

	 ;; big-re combines the sexp-res into one, so a single dfa can be built
	 (big-re (datum->syntax-object 
		  #'here
		  (cons ': (map (lambda (re)
				  (set! index (add1 index))
				  (list '@ re (make-marker index)))
				regs))
		  #f))

	 (ast (parse big-re))

	 ;; A vector with just the chars from the regex (in order)
	 ;; includes integer placeholders and eof
	 (chars (re-ast->chars ast))

	 (dfa (re-ast->dfa ast chars))

	 ;; A hash table that will map a state name (list of int) to an 
	 ;; integer
	 (state-numbering (make-hash-table))

	 ;; for each state, which action it is a final state for (-1 indicates
	 ;; that the state is not final)
	 (finals (make-vector (length (dfa-states dfa)) #f))

	 ;; The lexer table for transitions on eof
	 (eof-table (make-vector (length (dfa-states dfa)) #f))


	 ;; The lexer table, one entry per state per char.
	 ;; Each entry specifies a state to transition to.
	 ;; #f indicates no transition
	 (char-table (make-vector (* 256 (length (dfa-states dfa))) #f)))

    ;; Fill the state-numbering hash-table
    (set! index 0)
    (for-each (lambda (state-name)
		(hash-table-put! state-numbering
			 state-name
			 index)
		(set! index (add1 index)))
	      (dfa-states dfa))


    ;; Fill the char-table vector and eof-table vector
    (for-each 
     (lambda (trans)
       (for-each (lambda (to)
		   (cond
		    ((char? (car to))
		     (vector-set! char-table
				  (bitwise-ior 
				   (char->integer (car to))
				   (arithmetic-shift 
				    (hash-table-get 
				     state-numbering
				     (transition-from trans))
				    8))
				  (hash-table-get 
				   state-numbering
				   (cadr to))))
		    (else
		     (vector-set! eof-table
				  (hash-table-get 
				   state-numbering
				   (transition-from trans))
				  (hash-table-get 
				   state-numbering
				  (cadr to))))))
		 (transition-to trans)))
     (dfa-trans dfa))
    
    ;; Fill the finals vector
    (for-each (lambda (state-name)
		(let ((x (quicksort 
			  (map (lambda (m) (marker-rule-num m))
			       (filter marker?
				       (map (lambda (i) (vector-ref chars i))
					    state-name)))
			  <)))
		  (if (not (null? x))
		      (vector-set! finals 
				   (hash-table-get 
				    state-numbering 
				    state-name)
				   (vector-ref actions (car x))))))
              (dfa-states dfa))

    (let ((start-state (hash-table-get state-numbering 
				       (dfa-start dfa))))
      (if (vector-ref finals start-state)
	  (raise-syntax-error
	   'lex
	   "Generated lexer accepts the empty string"
	   s))
      (make-table char-table
		  eof-table
		  start-state
		  finals))))


)






