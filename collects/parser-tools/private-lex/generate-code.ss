(module generate-code mzscheme
  
  ;; creates the lexer
  
  (require "structs.ss"
	   "re-to-dfa.ss"
	   "sexp-to-re.ss"
	   (lib "list.ss"))
  
  (provide generate-table build-lexer)
  
  
  ;; build-lexer : syntax-object * s-expr -> syntax-object
  ;; has the lexer's runtime code as well as the initial compile-time driver
  (define (build-lexer runtime wrap)
    (let ((code
           `(letrec ((match
                      (lambda (lb first-pos longest-match-length longest-match-action length)
                        (let ((match
                               (push-back lb (- length longest-match-length)))
                              (end-pos (get-position lb)))
                          (if (not longest-match-action)
                              (raise-read-error
                               (format "lexer: No match found in input starting with: ~a"
                                       (list->string (filter char? (lex-buffer-from lb))))
                               #f
                               (position-line first-pos)
                               (position-col first-pos)
                               (position-offset first-pos)
                               (- (position-offset end-pos) (position-offset first-pos))))
                          (,wrap
                           (longest-match-action
                            (lambda ()
                              first-pos)
                            (lambda ()
                              end-pos)
                            (lambda ()
                              (if (char? (car match))
                                  (list->string (reverse match))
                                  (list->string (reverse (cdr match)))))
                            lb))))))
              (lambda (lb)
                (unless (lex-buffer? lb)
                  (raise-type-error 
                   'lexer 
                   "lex-buf"
                   0
                   lb))
                (let ((first-pos (get-position lb)))
                  (let lexer-loop (
                                   ;; current-state
                                   (state start-state)
                                   ;; the character to transition on
                                   (char (next-char lb))
                                   ;; action for the longest match seen thus far
                                   ;; including a match at the current state
                                   (longest-match-action 
                                    (vector-ref actions start-state))
                                   ;; how many characters have been read
                                   ;; including the one just read
                                   (length 1)
                                   ;; how many characters are in the longest match
                                   (longest-match-length 0))
                    (let ((next-state
                           (cond
                             ((eof-object? char)
                              (vector-ref eof-table state))
                             (else
                              (vector-ref 
                               trans-table
                               (bitwise-ior (char->integer char)
                                            (arithmetic-shift state 8)))))))
                      (cond
                        ((not next-state) (match lb
                                            first-pos
                                            longest-match-length
                                            longest-match-action
                                            length))
                        (else
                         (let ((act (vector-ref actions next-state)))
                           (lexer-loop next-state 
                                       (next-char lb)
                                       (if act
                                           act
                                           longest-match-action)
                                       (add1 length)
                                       (if act
                                           length
                                           longest-match-length))))))))))))
      (lambda (stx)
        (syntax-case stx ()
          ((_)
           (raise-syntax-error #f "empty lexer is not allowed" stx))
          ((_ re-act ...)
           (begin
             (for-each
              (lambda (x)
                (syntax-case x ()
                  ((re act) (void))
                  (_ (raise-syntax-error 'lexer 
                                         "expects regular expression / action pairs"
                                         x))))
              (syntax->list (syntax (re-act ...))))
             (let* ((table (generate-table (syntax (re-act ...)) stx))
                    (code
                     `(let ((start-state ,(table-start table))
                            (trans-table ,(table-trans table))
                            (eof-table ,(table-eof table))
                            (actions (vector ,@(vector->list (table-actions table)))))
                        ,code)))
               (datum->syntax-object runtime code #f))))))))
    
  
  
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
                           `(lambda (get-start-pos get-end-pos get-lexeme lex-buf)
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






