(module lex mzscheme

  ;; Provides the syntax used to create lexers and the functions needed to
  ;; create and use the buffer that the lexer reads from.  See doc.txt.
	
  (require-for-syntax "private-lex/generate-code.ss"
                      "private-lex/structs.ss")

  (require (lib "readerr.ss" "syntax")
           "private-lex/token.ss"
	   (lib "cffi.ss" "compiler"))

  (provide lexer lexer-src-pos define-lex-abbrev define-lex-abbrevs
	   position-offset position-line position-col position?
           define-tokens define-empty-tokens token-name token-value token? file-path)
  
  (define file-path (make-parameter #f))
  
  #;(define-syntaxes (lexer-exp lexer-src-pos-exp)
    (let ((build-lexer
           (lambda (wrap?)
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
                    (let ((table (generate-table (syntax (re-act ...)) stx)))
                      (with-syntax ((code (compile-table table))
                                    (actions-stx `(vector ,@(vector->list (table-actions table))))
                                    (wrap? wrap?))
                        (syntax
                         (compiled-lexer-body code actions-stx wrap?)))))))))))
      (values
       (build-lexer #f)
       (build-lexer #t))))

  (define-syntaxes (lexer lexer-src-pos)
    (let ((build-lexer
	   (lambda (wrap?)
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
                    (let* ((spec/re-act-lst
                            (syntax->list (syntax (re-act ...))))
                           (spec-act 
                            (get-special-action spec/re-act-lst 'special #'(void)))
                           (spec-error-act
                            (get-special-action spec/re-act-lst 'special-error #'(raise exception)))
                           (spec-comment-act 
                            (get-special-action spec/re-act-lst 'special-comment #'#f))
                           (re-act-lst
                            (filter-out-specials spec/re-act-lst
                                                 '(special special-comment special-error))))
                      (let ((table (generate-table re-act-lst #'here stx)))
                        (with-syntax ((start-state-stx (table-start table))
                                      (trans-table-stx (table-trans table))
                                      (eof-table-stx (table-eof table))
                                      (no-lookahead-stx (table-no-lookahead table))
                                      (actions-stx `(vector ,@(vector->list (table-actions table))))
                                      (spec-act-stx
                                       (wrap-action spec-act 'special #'here spec-act))
                                      (spec-error-act-stx
                                       (wrap-action spec-error-act 'exception #'here spec-error-act))
                                      (has-comment-act?-stx (if (syntax-e spec-comment-act) #t #f))
                                      (spec-comment-act-stx
                                       (wrap-action spec-comment-act (gensym) #'here spec-comment-act))
                                      (wrap? wrap?))
                          (syntax 
                           (lexer-body start-state-stx 
                                       trans-table-stx
                                       eof-table-stx
                                       actions-stx
                                       no-lookahead-stx
                                       spec-act-stx
                                       spec-error-act-stx
                                       has-comment-act?-stx
                                       spec-comment-act-stx
                                       wrap?))))))))))))
      (values
       (build-lexer #f)
       (build-lexer #t))))
    
  (define-syntax (define-lex-abbrev stx)
    (syntax-case stx ()
      ((_ name re)
       (syntax
        (define-syntax name
          (make-lex-abbrev (quote-syntax re)))))
      (_ 
       (raise-syntax-error
        #f
        "Form should be (define-lex-abbrev name re)"
        stx))))

  (define-syntax (define-lex-abbrevs stx)
    (syntax-case stx ()
      ((_ x ...)
       (let* ((abbrev (syntax->list (syntax (x ...))))
              (r (map (lambda (a)
                        (syntax-case a ()
                          ((name re)
                           (identifier? (syntax name))
                           (syntax (define-lex-abbrev name re)))
                          (_ (raise-syntax-error
                              'Lexer-abbreviation 
                              "Form should be (identifier value)"
                              a))))
                      abbrev)))
         (datum->syntax-object
          #'here
          (cons 'begin r)
          stx)))
      (_
       (raise-syntax-error
        #f
        "Form should be (define-lex-abbrevs (name re) ...)"
        stx))))

  #;(define (compiled-lexer-body lexer actions wrap?)
    (lambda (ip)
      (unless (input-port? ip)
        (raise-type-error 
         'lexer 
         "input-port"
         0
         ip))
      (let ((first-pos (get-position ip)))
        (let-values (((longest-match-length length longest-match-action)
                      (lexer ip peek-string)))
          (check-match ip first-pos longest-match-length length
                       (vector-ref actions longest-match-action) wrap?)))))
  
  (define (lexer-body start-state trans-table eof-table actions no-lookahead
                      special-action special-error-action
                      has-special-comment-action? special-comment-action wrap?)
    (letrec ((lexer
              (lambda (ip)
                (unless (input-port? ip)
                  (raise-type-error 
                   'lexer 
                   "input-port"
                   0
                   ip))
                (let ((first-pos (get-position ip))
                      (first-char (peek-char-or-special ip 0)))
                  (cond
                    ((eq? 'special first-char)
                     (let* ((comment? #f)
                            (error? #f)
                            (spec (with-handlers ((exn:special-comment?
                                                   (lambda (x) (set! comment? #t)))
                                                  (not-break-exn?
                                                   (lambda (ex) (set! error? #t) ex)))
                                    (read-char-or-special ip))))
                       (cond
                         ((and comment? (not has-special-comment-action?))
                          (lexer ip))
                         (else
                          (do-match ip first-pos (cond
                                                   (comment? special-comment-action)
                                                   (error? special-error-action)
                                                   (else special-action))
                                    spec wrap?)))))
                    (else
                     (let lexer-loop (
                                      ;; current-state
                                      (state start-state)
                                      ;; the character to transition on
                                      (char first-char)
                                      ;; action for the longest match seen thus far
                                      ;; including a match at the current state
                                      (longest-match-action 
                                       (vector-ref actions start-state))
                                      ;; how many characters have been read
                                      ;; including the one just read
                                      (length 1)
                                      ;; how many characters are in the longest match
                                      (longest-match-length 1))
                       (let ((next-state 
                              (cond
                                ((eof-object? char)
                                 (vector-ref eof-table state))
                                ((eq? char 'special)
                                 #f)
                                (else
                                 (vector-ref 
                                  trans-table
                                  (+ (char->integer char) (* 256 state)))))))
                         (cond
                           ((not next-state)
                            (check-match ip first-pos longest-match-length
                                         length longest-match-action wrap?))
                           ((vector-ref no-lookahead next-state)
                            (let ((act (vector-ref actions next-state)))
                              (check-match ip 
                                           first-pos 
                                           (if act length longest-match-length)
                                           length
                                           (if act act longest-match-action)
                                           wrap?)))
                           (else
                            (let ((act (vector-ref actions next-state)))
                              (lexer-loop next-state 
                                          (peek-char-or-special ip length)
                                          (if act
                                              act
                                              longest-match-action)
                                          (add1 length)
                                          (if act
                                              length
                                              longest-match-length)))))))))))))
      lexer))
      
  (define id (lambda (x) x))

  (define (check-match lb first-pos longest-match-length length longest-match-action wrap?)
    (unless longest-match-action
      (let* ((match (read-string length lb))
	     (end-pos (get-position lb)))
	(raise-read-error
	 (format "lexer: No match found in input starting with: ~a" match)
	 (file-path)
	 (position-line first-pos)
	 (position-col first-pos)
	 (position-offset first-pos)
	 (- (position-offset end-pos) (position-offset first-pos)))))
    (let ((match (read-string longest-match-length lb)))
      (do-match lb first-pos longest-match-action match wrap?)))
      
      
  (define (do-match ip first-pos action value wrap?)
    (let ((end-pos (get-position ip)))
      (cond
        (wrap?
         (let/ec ret
           (list (action first-pos end-pos value ret ip)
                 first-pos 
                 end-pos)))
        (else 
         (action first-pos end-pos value id ip)))))
  
  (define-struct position (offset line col))
  (define (get-position ip)
    (let-values (((line col off) (port-next-location ip)))
      (make-position off line col)))

)
