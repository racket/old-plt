#cs
(module beginner-error mzscheme
  
  (require "lexer.ss" "general-parsing.ss")
  
  (provide find-beginner-error find-beginner-error-interactions)
  
  ;find-error: port -> (U void #t)
  (define (find-beginner-error port)
    (port-count-lines! port)
    (file-position port 0)
    (let ((getter (lambda () (get-token port))))
      (parse-definition (getter) 'start getter)))

  (define (raise-error msg token token2)
    (printf "Error: ~a (~a, ~a)" msg token token2)
    (error msg))

  ;find-error-interaction: port -> void
  (define (find-beginner-error-interactions port)
    (port-count-lines! port)
    (file-position port 0)
    (let* ((getter (lambda () (get-token port)))
           (first-tok (getter)))
      (case (get-token-name (car first-tok))
        ((EOF) (void))
        ((if return) (parse-statement first-tok 'start getter))
        (else (parse-expression first-tok 'start getter)))
      #t))
      
  
  ;parse-definition: token symbol (-> token) -> void
  (define (parse-definition cur-tok state getter)
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      
      (case state
        ((start) 
         (case tok-kind
           ((EOF) #t);(error 'parse-definition-beginner "Internal Error: Unexpectedly Succeeded in presence of error"))
           ((class) (parse-definition (getter) 'class-id getter))
           ((abstract) 
            (let ((next-tok (getter)))
              (cond
                ((class? (car next-tok)) (parse-definition next-tok state getter))
                ((eof? (car next-tok)) (raise-error 'eof-instead-of-class tok next-tok))
                (else (raise-error 'abstract-at-top-level-not-followed-by-class tok next-tok)))))
           (else (raise-error 'class-not-found-at-top-level tok null))))
        ((class-id)
         (case tok-kind
           ((EOF) (raise-error 'eof-instead-of-classname tok null))
           ((IDENTIFIER) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'eof-after-class-name tok next-tok))
                ((extends? (car next-tok)) (parse-definition next-tok 'extends getter))
                ((o-brace? (car next-tok)) (parse-definition next-tok 'body getter))
                (else (raise-error 'brace-or-extends-not-found-after-class-id tok next-tok)))))
           (else (raise-error 'class-identifier-not-found tok null))))
        ((extends) 
         (let ((next-tok (getter)))
           (cond
             ((eof? (car next-tok)) (raise-error 'eof-after-extend tok next-tok))
             ((id-token? (car next-tok)) (parse-definition (getter) 'body getter))
             ((o-brace? (car next-tok)) (raise-error 'got-o-brace-after-extend-not-name tok next-tok))
             (else (raise-error 'extends-identifier-not-found tok next-tok)))))
        ((body)
         (case tok-kind
           ((EOF) (raise-error 'eof-instead-of-classbody tok null))
           ((O_BRACE) (parse-definition (parse-members (getter) 'start getter) 'body-end getter))
           (else (raise-error 'body-not-started-with-o-brace tok null))))
        ((body-end)
         (case tok-kind
           ((C_BRACE) 
            (let ((next-tok (getter)))
              (cond
                ((or (eof? (car next-tok)) (class? (car next-tok)) (abstract? (car next-tok)))
                 (parse-definition next-tok 'start getter))
                (else (raise-error 'expected-new-class-or-nothing-after-body tok next-tok)))))
           (else (raise-error 'expected-a-c-brace-to-end-closest-class tok null)))))))
              
  ;parse-members: token symbol (->token) -> token
  (define (parse-members cur-tok state getter) 
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))

      (case state
        ((start)
         (cond
           ((or (eof? tok) (c-brace? tok)) cur-tok)
           ((abstract? tok) (parse-members (getter) 'method getter))
           ((prim-type? tok) (parse-members (getter) 'method-or-field getter))
           ((id-token? tok) (parse-members cur-tok 'member getter))
           (else (raise-error 'invalid-start-of-member tok null))))
        ((member)
         (let ((next-tok (getter)))
           (cond
             ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
             ((dot? (car next-tok)) (parse-members (parse-name (getter) getter) 'method-or-field getter))
             ((id-token? (car next-tok)) (parse-members next-tok 'method-or-field getter))
             ((o-paren? (car next-tok)) (parse-members (getter) 'ctor-parms getter))
             (else (raise-error 'invalid-second-of-member tok next-tok)))))
        ((method-or-field)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((IDENTIFIER) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ;Just ended a field
                ((semi-colon? (car next-tok)) (parse-members (getter) 'start getter))
                ((comma? (car next-tok)) (raise-error 'field-end-in-semicolon tok next-tok))
                ((o-paren? (car next-tok)) (parse-members (getter) 'method-parms getter))
                (else (raise-error 'invalid-field-or-method tok next-tok)))))
           (else (raise-error 'expected-id-for-field-or-method tok null))))
        ((method)
         (cond
           ((prim-type? tok) (parse-members (getter) 'method-id getter))
           ((id-token? tok)
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((dot? (car next-tok)) (parse-members (parse-name (getter) getter) 'method-id getter))
                ((o-paren? (car next-tok)) (raise-error 'abstract-ctor tok next-tok))
                ((semi-colon? (car next-tok)) (raise-error 'abstract-field tok next-tok))
                ((id-token? (car next-tok)) (parse-members next-tok 'method-id getter))
                (else (raise-error 'expected-method-name tok next-tok)))))
           (else (raise-error 'expected-method-type tok null))))
        ((method-id)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((IDENTIFIER)
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((o-paren? (car next-tok)) (parse-members (getter) 'method-parms getter))
                ((semi-colon? (car next-tok)) 
                 (raise-error 'abstract-field tok next-tok))
                (else (raise-error 'invalid-method-no-parms tok next-tok)))))
           (else (raise-error 'expected-method-name tok null))))
        ((ctor-parms)
         (cond
           ((eof? tok) (raise-error 'invalid-end-of-input tok null))
           ((c-paren? tok)
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((o-brace? (car next-tok)) 
                 (parse-members (parse-ctor-body (getter) 'start getter) 'ctor-end getter))
                ((semi-colon? (car next-tok)) (raise-error 'no-body-for-ctor tok next-tok))
                (else (raise-error 'invalid-body tok next-tok)))))
           ((or (prim-type? tok) (id-token? tok))
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((comma? (car next-tok)) (raise-error 'expected-var-name-after-type tok next-tok))
                ((c-paren? (car next-tok)) (raise-error 'expected-var-name-after-type tok next-tok))
                ((id-token? (car next-tok))
                 (let ((after-id (getter)))
                   (cond
                     ((eof? (car after-id)) (raise-error 'invalid-end-of-input tok next-tok))
                     ((c-paren? (car after-id)) (parse-members after-id 'ctor-parms getter))
                     ((comma? (car after-id))
                      (let ((after-comma (getter)))
                        (cond
                          ((eof? (car after-comma)) (raise-error 'invalid-end-of-input tok next-tok))
                          ((c-paren? (car after-comma)) (raise-error 'variable-must-follow-comma after-id after-comma))
                          ((comma? (car after-comma)) (raise-error 'comma-already-present-new-one-not-needed tok after-comma))
                          (else (parse-members after-comma 'ctor-parms getter)))))
                     ((or (prim-type? (car after-id)) (id-token? (car after-id)))
                      (raise-error 'comma-needed-between-identifiers next-tok after-id))
                     (else (raise-error 'expected-comma-or-c-paren tok after-id)))))
                (else (raise-error 'expected-var-name-after-type tok next-tok)))))
           (else (raise-error 'expected-var-id-or-paren tok null))))
        ((method-parms)
         (cond
           ((eof? tok) (raise-error 'invalid-end-of-input tok null))
           ((c-paren? tok)
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((o-brace? (car next-tok)) 
                 (parse-members (parse-statement (getter) 'start getter) 'method-end getter))
                ((semi-colon? (car next-tok)) (parse-members (getter) 'start getter))
                (else (raise-error 'invalid-body tok next-tok)))))
           ((or (prim-type? tok) (id-token? tok))
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input tok next-tok))
                ((comma? (car next-tok)) (raise-error 'expected-var-name-after-type tok next-tok))
                ((c-paren? (car next-tok)) (raise-error 'expected-var-name-after-type tok next-tok))
                ((id-token? (car next-tok))
                 (let ((after-id (getter)))
                   (cond
                     ((eof? (car after-id)) (raise-error 'invalid-end-of-input tok next-tok))
                     ((c-paren? (car after-id)) (parse-members after-id 'method-parms getter))
                     ((comma? (car after-id))
                      (let ((after-comma (getter)))
                        (cond
                          ((eof? (car after-comma)) (raise-error 'invalid-end-of-input tok next-tok))
                          ((c-paren? (car after-comma)) (raise-error 'variable-must-follow-comma after-id after-comma))
                          ((comma? (car after-comma)) (raise-error 'comma-already-present-new-one-not-needed tok after-comma))
                          (else (parse-members after-comma 'method-parms getter)))))
                     ((or (prim-type? (car after-id)) (id-token? (car after-id)))
                      (raise-error 'comma-needed-between-identifiers next-tok after-id))
                     (else (raise-error 'expected-comma-or-c-paren tok after-id)))))
                (else (raise-error 'expected-var-name-after-type tok next-tok)))))
           (else (raise-error 'expected-var-id-or-paren tok null))))
        ((ctor-end)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((C_BRACE) (parse-members (getter) 'start getter))
           (else (raise-error 'expected-end-of-ctor-body tok null))))
        ((method-end)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((C_BRACE) (parse-members (getter) 'start getter))
           (else (raise-error 'expected-end-of-method-body tok null)))))))
  
  ;parse-type: token (-> token) -> token
  (define (parse-name cur-tok getter) 
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      (case tok-kind
        ((IDENTIFIER) 
         (let ((next-tok (getter)))
           (if (dot? (car next-tok))
               (parse-name (getter) 'start getter)
               next-tok)))
        ((PERIOD)
         (raise-error 'cannot-have-two-dots-after-each-other tok null))
        (else
         (raise-error 'expected-identifier-after-dot tok null)))))
  
  ;parse-body: token symbol (->token) -> token
  (define (parse-ctor-body cur-tok state getter)
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      (case state
        ((start)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((C_BRACE) cur-tok)
           ((this) 
            (let ((next-token (getter)))
              (if (dot? (car next-token))
                  (let ((after-dot (getter)))
                    (if (id-token? (car after-dot))
                        (parse-ctor-body (getter) 'assign-op getter)
                        (raise-error 'id-should-follow-dot tok after-dot)))
                  (raise-error 'dot-id-should-follow-this tok next-token))))
           ((IDENTIFIER)
            (let ((next-token (getter)))
              (if (dot? (car next-token)) 
                  (parse-ctor-body (parse-name (getter) getter) 'assign-op getter)
                  (parse-ctor-body next-token 'assign-op getter))))
           (else (raise-error 'invalid-in-ctor-body tok null))))
        ((assign-op)
         (case tok-kind
           ((=) (parse-ctor-body (parse-expression (getter) 'start getter) 'assign-end getter))
           (else (raise-error 'expected-equal-for-assignment tok null))))
        ((assign-end)
         (if (semi-colon? tok)
             (parse-ctor-body (getter) 'start getter)
             (raise-error 'assignment-must-end-with-semi-colon tok null))))))

  ;parse-statement: token symbol (->token) -> token
  (define (parse-statement cur-tok state getter)
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      (case state
        ((start)
         (case tok-kind
           ((if) (parse-statement (getter) 'if getter))
           ((return) (parse-statement (parse-expression (getter) 'start getter) 'return getter))
           (else cur-tok)))
        ((if)
         (case tok-kind
           ((EOF) (raise-error 'unexpexted-end-of-input tok null))
           ((O_PAREN) (parse-statement (parse-expression (getter) 'start getter) 'if-then getter))
           (else (raise-error 'if-must-be-followed-by-expression-in-parens tok null))))
        ((if-then)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input tok null))
           ((C_PAREN) (parse-statement (parse-statement (getter) 'start getter) 'if-else getter))
           (else (raise-error 'if-must-be-followed-by-expression-in-parens-parens-not-match tok null))))
        ((if-else)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input tok null))
           ((else) (parse-statement (parse-statement (getter) 'start getter) 'start getter))
           (else (raise-error 'if-then-statement-must-have-else tok null))))
        ((return)
         (case tok-kind
           ((SEMI_COLON) (parse-statement (getter) 'start getter))
           (else (raise-error 'return-must-end-with-semi-colon tok null)))))))
  
  ;parse-expression: token state (->token) -> token
  (define (parse-expression cur-tok state getter) 
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      (case state
        ((start)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input tok null))
           ((~ !) (parse-expression (parse-expression (getter) 'start getter) 'op-or-end getter))
           ((NULL_LIT TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this)
            (parse-expression (getter) 'dot-op-or-end getter))
           ((O_PAREN)
            (parse-expression (parse-expression (getter) 'start getter) 'c-paren getter))
           ((new) (parse-expression (getter) 'class-alloc-start getter))
           ((IDENTIFIER) (parse-expression (getter) 'name getter))
           (else 'invalid-start-of-expression tok null)))
        ((op-or-end)
         (if (bin-operator? tok)
             (parse-expression (getter) 'start getter)
             cur-tok))
        ((dot-op-or-end)
           (cond
             ((dot? tok) 
              (let ((next-tok (getter)))
                (if (id-token? (car next-tok))
                    (parse-expression (getter) 'method-call-args getter)
                    (raise-error 'expected-id-after-dot-after-primary tok next-tok))))
             ((bin-operator? tok)
              (parse-expression (getter) 'start getter))
             (else cur-tok)))
        ((c-paren)
         (if (c-paren? tok)
             (parse-expression (getter) 'dot-op-or-end getter)
             (raise-error 'o-paren-expected-to-be-closed-at-end-of-expression tok null)))
        ((class-alloc-start)
         (if (id-token? tok)
             (let ((next-token (getter)))
               (cond
                 ((dot? (car next-token)) 
                  (parse-expression (parse-name (getter) getter) 'class-args-start getter))
                 ((o-paren? (car next-token)) (parse-expression next-token 'class-args-start getter))
                 (else 'must-give-ctor-args-to-create-class-instance tok next-token)))
             (raise-error 'class-alloc-must-have-identifier tok null)))
        ((class-args-start)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((O_PAREN) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) 'invalid-end-of-input tok null)
                ((c-paren? (car next-tok)) (parse-expression (getter) 'dot-op-or-end getter))
                (else (parse-expression (parse-expression next-tok 'start getter) 'class-args getter)))))
           (else (raise-error 'need-ctor-args tok null))))
        ((class-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((C_PAREN) (parse-expression (getter) 'dot-op-or-end getter))
           ((COMMA) (parse-expression (parse-expression (getter) 'start getter) 'class-args getter))
           (else (raise-error 'need-comma-between-ctor-args tok null))))
        ((name)
         (case tok-kind
           ((PERIOD) (parse-expression (parse-name (getter) getter) 'name getter))
           ((O_PAREN) (parse-expression cur-tok 'method-call-args getter))
           (else (parse-expression cur-tok 'op-or-end getter))))
        ((method-call-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((O_PAREN) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) 'invalid-end-of-input tok null)
                ((c-paren? (car next-tok)) (parse-expression (getter) 'dot-op-or-end getter))
                (else (parse-expression (parse-expression next-tok 'start getter) 'method-args getter)))))
           (else (raise-error 'need-call-args tok null))))
        ((method-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input tok null))
           ((C_PAREN) (parse-expression (getter) 'dot-op-or-end getter))
           ((COMMA) (parse-expression (parse-expression (getter) 'start getter) 'method-args getter))
           (else (raise-error 'need-comma-between-method-args tok null)))))))

      
  )
  
 