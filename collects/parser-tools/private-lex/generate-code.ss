(module generate-code mzscheme
  
  ;; creates the lexer
  
  (require "structs.ss"
	   "re-to-dfa.ss"
	   "sexp-to-re.ss"
	   (lib "list.ss"))
  
  (provide generate-table get-special-action filter-out-specials)
  
  ;; get-special-action: (syntax-object list) symbol 'a -> syntax-object or 'a
  ;; Returns the first action from a rule of the form ((which-special) action)
  (define (get-special-action rules which-special none)
    (cond
      ((null? rules) none)
      (else
       (syntax-case (car rules) ()
         (((special) act)
          (eq? (syntax-e (syntax special)) which-special)
          (syntax act))
         (_ (get-special-action (cdr rules) which-special none))))))
  
  ;; filter-out-specials: (syntax-object list) (symbol list) -> (syntax-object list)
  ;; Returns a list missing all the rules of the form ((special) action)
  ;; where special is a symbol in which specials.
  (define (filter-out-specials rules which-specials)
    (cond
      ((null? rules) null)
      (else
       (syntax-case (car rules) ()
         (((special) act)
          (memq (syntax-e (syntax special)) which-specials)
          (filter-out-specials (cdr rules) which-specials))
         (_ (cons (car rules) (filter-out-specials (cdr rules) which-specials)))))))
  
  ;; generate-table : (syntax-object list) syntax-object -> lexer-table
  ;; Creates the lexer's tables from a list of sexp-regex, action pairs.
  (define (generate-table rules s)
    (let* (
           ;; A counter
           (index -1)
           
           (regs (map (lambda (x) (car (syntax->list x))) rules))
           
           (actions (list->vector 
                     (map 
                      (lambda (x) 
                        (let ((action (cadr (syntax->list x))))
                          (datum->syntax-object
                           action
                           `(lambda (start-pos end-pos lexeme return-without-pos input-port)
                              ,action)
                           action)))
                      rules)))
           
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
           
           ;; For each state whether the lexer can ignore the next input.
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
  
  #;(define (compile-table table)
    (let ((code (build-code table)))
      `(c-lambda (scheme-object scheme-object) ;; input-port peek-string
		 scheme-object 
		 ,code)))
  
  
  #;(define (build-code table)
    (let* ((trans (table-trans table))
	   (eof (table-eof table))
	   (start-state (table-start table))
	   (actions (table-actions table))
	   (no-look (table-no-lookahead table)))
      (string-append
       (format "Scheme_Object* char_in;~n")
       (format "Scheme_Object* res[3];~n")
       (format "Scheme_Object* peekarg[3];~n")
       (format "int longest_match_length, longest_match_action, length;~n")
       (format "longest_match_action = ~a;~n" start-state)
       (format "longest_match_length = 1;~n")
       (format "length = 0;~n")
       (format "peekarg[0] = scheme_make_integer(1);~n")
       (format "peekarg[2] = ___arg1;~n")
       (format "goto scheme_lexer_~a;~n" start-state)
       (let loop ((current-state 0))
	 (cond
           ((< current-state (vector-length eof))
            (string-append
             (format "scheme_lexer_~a:~n" current-state)
             (if (vector-ref actions current-state)
                 (string-append
                  (format "  longest_match_action = ~a;~n" current-state)
                  (format "  longest_match_length = length;~n"))
                 "")
	     (format "  peekarg[1] = scheme_make_integer(length);~n")
             (format "  char_in = _scheme_apply(___arg2, 3, peekarg);~n")
             (format "  length = length + 1;~n")
             (format "  switch ((char_in != scheme_eof) ? (SCHEME_STR_VAL(char_in))[0] : 256)~n  {~n")
	     
             (let ((cases
                    (let loop ((current-char 0))
                      (cond
                        ((< current-char 257)
                         (let ((next-state
                                (cond
                                  ((< current-char 256) 
                                   (vector-ref trans (+ current-char (* 256 current-state))))
                                  (else 
                                   (vector-ref eof current-state)))))
                           (cond
                             ((not next-state) 
                              (cons `(,current-char #f)
                                    (loop (add1 current-char))))
                             ((vector-ref no-look next-state)
                              (cons `(,current-char ("end" ,next-state))
                                    (loop (add1 current-char))))
                             (else
                              (cons `(,current-char ,next-state)
                                    (loop (add1 current-char)))))))
                        (else null))))
                   (no-looks (make-vector (vector-length eof) null))
                   (gotos (make-vector (vector-length eof) null)))
               
               (let loop ((cases cases))
                 (cond
                   ((not (null? cases))
                    (let ((trans (car cases)))
                      (cond
                        ((not (cadr trans)) (void))
                        ((pair? (cadr trans))
                         (vector-set! no-looks (cadr (cadr trans)) 
                                      (cons (car trans)
                                            (vector-ref no-looks (cadr (cadr trans))))))
                        (else
                         (vector-set! gotos (cadr trans)
                                      (cons (car trans)
                                            (vector-ref gotos (cadr trans))))))
                      (loop (cdr cases))))))
               
	       (let loop ((i 0))
		 (if (< i (vector-length eof))
		     (begin
		       (vector-set! no-looks i (reverse (vector-ref no-looks i)))
		       (vector-set! gotos i (reverse (vector-ref gotos i)))
		       (loop (add1 i)))))

               (let loop ((goto-state 0))
                 (cond
                   ((< goto-state (vector-length eof))
                    (string-append
                     (apply string-append 
                            (map (lambda (char)
                                   (format "  case ~a:~n" char))
                                 (vector-ref gotos goto-state)))
                     (if (not (null? (vector-ref gotos goto-state)))
                         (format "    goto scheme_lexer_~a;~n" goto-state)
                         "")
                     (apply string-append
                            (map (lambda (char)
                                   (format "  case ~a:~n" char))
                                 (vector-ref no-looks goto-state)))
                     (if (not (null? (vector-ref no-looks goto-state)))
                         (string-append
                          (format "    longest_match_length = length;~n")
                          (format "    longest_match_action = ~a;~n" goto-state)
                          (format "    goto scheme_lexer_end;~n"))
                         "")
                     (loop (add1 goto-state))))
                   (else ""))))
             
             (format "  default:~n")
             (format "    goto scheme_lexer_end;~n")
             (format "  }~n")
             (loop (add1 current-state))))
           (else "")))
       (format "scheme_lexer_end:~n")
       (format "  res[0] = scheme_make_integer(longest_match_length);~n")
       (format "  res[1] = scheme_make_integer(length);~n")
       (format "  res[2] = scheme_make_integer(longest_match_action);~n")
       (format "  ___result = scheme_values(3, res);~n")
       )))
  
  )
