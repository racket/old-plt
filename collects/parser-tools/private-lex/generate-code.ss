(module generate-code mzscheme
  
  ;; creates the lexer
  
  (require "structs.ss"
	   "re-to-dfa.ss"
	   "sexp-to-re.ss"
	   (lib "list.ss"))
  
  (provide generate-table compile-table)
  
  ;; generate-table : syntax-object -> lexer-table
  ;; Creates the lexer's tables from a list of sexp-regex, action pairs.
  (define (generate-table rules s)
    (let* (
           ;; A counter
           (index -1)
           
           (regs (map (lambda (x) (car (syntax->list x))) (syntax->list rules)))
           
           (actions (list->vector 
                     (map 
                      (lambda (x) 
                        (let ((action (cadr (syntax->list x))))
                          (datum->syntax-object
                           action
                           `(lambda (get-start-pos get-end-pos get-lexeme return-without-pos lex-buf)
                              ,action)
                           action)))
                      (syntax->list rules))))
           
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
           
           ;; For each state whether the can ignore the next input
           ;; It can do this only if there are no transitions out of the
           ;; current state.
           (no-look (make-vector (length (dfa-states dfa)) #t))
           
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
         (let ((from-number (hash-table-get 
                             state-numbering
                             (transition-from trans))))
           (if (not (null? (transition-to trans)))
               (vector-set! no-look from-number #f))
           (for-each (lambda (to)
                       (cond
                         ((char? (car to))
                          (vector-set! char-table
                                       (bitwise-ior 
                                        (char->integer (car to))
                                        (arithmetic-shift from-number 8))
                                       (hash-table-get 
                                        state-numbering
                                        (cadr to))))
                         (else
                          (vector-set! eof-table
                                       from-number
                                       (hash-table-get 
                                        state-numbering
                                        (cadr to))))))
                     (transition-to trans))))
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
                    finals
                    no-look))))
  
  (require (lib "cffi.ss" "compiler"))

  (define (compile-table table)
    (with-syntax ((code (build-code table)))
      (syntax (c-lambda (scheme-object scheme-object) ;; lex-buf get-next-char
			scheme-object 
			code))))
			
  
  (define (build-code table)
    (let ((trans (table-trans table))
	  (eof (table-eof table))
	  (start-state (table-start table))
	  (actions (table-actions table))
	  (no-look (table-no-lookahead table)))
      (string-append
       (format "Scheme_Object* char_in;~n")
       (format "Scheme_Object* res[2];~n")
       (format "int longest_match_length, longest_match_action, length;~n")
       (format "longest_match_action = ~a;~n" start-state)
       (format "longest_match_length = length = 1;~n")
       (format "goto scheme_lexer_~a;~n" start-state)
       (let loop ((current-state 0))
	 (cond
	  ((< current-state (vector-length eof))
	   (string-append
	    (format "scheme_lexer_~a:~n" current-state)
	    (format "  char_in = scheme_apply(___arg2, 1, &___arg1);~n")
	    (format "  switch ((char_in == scheme_eof) ? SCHEME_CHAR_VAL(SCHEME_STRING_VAL(char_in)[0]) : 256)~n  {~n")
	    (let loop ((current-char 0))
	      (cond
	       ((< current-char 257)
		(let ((next-state 
		       (cond
			((< current-char 256) (vector-ref trans (+ current-char (* 256 current-state))))
			(else (vector-ref eof current-state)))))
		  (string-append
                   (cond
                     ((not next-state) "")
                     ((vector-ref no-look next-state)
                      (let ((act (vector-ref actions next-state)))
                        (if act
                            (string-append
                             (format "  case ~a:~n" current-char)
                             (format "    longest_match_length = length;~n")
                             (format "    longest_match_action = ~a;~n" next-state)
                             (format "    goto scheme_lexer_end;~n"))
                            (string-append
                             (format "  case ~a:~n" current-char)
                             (format "    goto scheme_lexer_end;~n")))))
                     (else
                      (let ((act (vector-ref actions next-state)))
                        (if act
                            (string-append
                             (format "  case ~a:~n" current-char)
                             (format "    longest_match_length = length;~n")
                             (format "    length = length + 1;~n")
                             (format "    longest_match_action = ~a;~n" next-state)
                             (format "    goto scheme_lexer_~a;~n" next-state))
                            (string-append
                             (format "  case ~a:~n" current-char)
                             (format "    length = length + 1;~n")
                             (format "    goto scheme_lexer_~a;~n" next-state))))))
		   (loop (add1 current-char)))))
	       (else (format ""))))
	    (format "  default:~n")
	    (format "    goto scheme_lexer_end;~n")
	    (format "  }~n")
	    (loop (add1 current-state))))
	  (else "")))
       (format "scheme_lexer_end:~n")
       (format "  res[1] = scheme_make_integer(longest_match_length);~n")
       (format "  res[2] = scheme_make_integer(longest_match_action);~n")
       (format "  ___result = scheme_values(2, res);~n")
       )))

  )
