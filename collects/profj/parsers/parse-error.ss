#cs
(module parse-error mzscheme
  
  (require "lexer.ss" "general-parsing.ss"
           "../parameters.ss"
           (lib "readerr.ss" "syntax")
           (lib "lex.ss" "parser-tools"))
  
  (provide find-intermediate-error find-intermediate-error-interactions 
           find-beginner-error find-beginner-error-interactions)

  (define level (make-parameter 'beginner))
  
  ;find-error: -> (U void #t)
  (define (find-beginner-error)
    (let ((port ((parse-error-port))))
      (port-count-lines! port)
      (let ((getter (lambda () (get-token port))))
        (parse-definition null (getter) 'start getter))))

  ;find-error-interaction: -> (U bool or token)
  ;Should not return
  (define (find-beginner-error-interactions)
    (let ((port ((parse-error-port))))
      (port-count-lines! port)
      (let* ((getter (lambda () (get-token port)))
             (first-tok (getter)))
        (let ((returned-tok 
               (case (get-token-name (car first-tok))
                 ((EOF) #t)
                 ((if return) (parse-statement null first-tok 'start getter #f))
                 ;Taken from Intermediate to allow interaction to say int x = 4;
                 ((IDENTIFIER)
                  (let ((next (getter)))
                    (if (id-token? (get-tok next))
                        (parse-field first-tok next 'start getter)
                        (parse-expression first-tok next 'name getter))))
                 (else
                  (if (prim-type? (get-tok first-tok))
                      (parse-field first-tok (getter) 'start getter)
                      (parse-expression null first-tok 'start getter))))))
          
          (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
              returned-tok
              (if (and (pair? returned-tok) (semi-colon? (get-tok returned-tok)))
                  (parse-error "';' is not allowed here" (get-start returned-tok) (get-end returned-tok))
                  (parse-error (format "Only 1 statement, expression, or definition is allowed, found extra input ~a"
                                       (output-format (get-tok returned-tok)))
                               (get-start returned-tok) (get-end returned-tok))))))))
                 
;                 (else (parse-expression null first-tok 'start getter)))))
;          (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
;              returned-tok
;              (parse-error (format "Only 1 statement or expression is allowed, found extra input ~a" 
;                                   (output-format (get-tok returned-tok)))
;                           (get-start returned-tok) (get-end returned-tok)))))))
  
  ;find-error: -> (U void #t)
  (define (find-intermediate-error)
    (let ((port ((parse-error-port))))
      (port-count-lines! port)
      (let ((getter (lambda () (get-token port))))
        (level 'intermediate)
        (parse-definition null (getter) 'start getter))))

  ;find-error-interaction: -> (U bool or token)
  ;Should not return
  (define (find-intermediate-error-interactions)
    (let ((port ((parse-error-port))))
      (port-count-lines! port)
      (let* ((getter (lambda () (get-token port)))
             (first-tok (getter)))
        (level 'intermediate)
        (let ((returned-tok 
               (case (get-token-name (get-tok first-tok))
                 ((EOF) #t)
                 ((if return O_BRACE) (parse-statement null first-tok 'start getter #t))
                 ((IDENTIFIER)
                  (let ((next (getter)))
                    (if (id-token? (get-tok next))
                        (parse-statement first-tok next 'local getter #t)
                        (parse-expression first-tok next 'name getter)))) 
                 (else 
                  (if (prim-type? (get-tok first-tok))
                      (parse-statement null first-tok 'start getter #t)
                      (parse-expression null first-tok 'start getter))))))
          (if (or (and (pair? returned-tok) (eof? (get-tok returned-tok))) (boolean? returned-tok))
              returned-tok
              (if (and (pair? returned-tok) (semi-colon? (get-tok returned-tok)))
                  (parse-error "';' is not allowed here" (get-start returned-tok) (get-end returned-tok))
                  (parse-error (format "Only 1 statement or expression is allowed, found extra input ~a" 
                                       (output-format (get-tok returned-tok)))
                               (get-start returned-tok) (get-end returned-tok))))))))
  
  

  (define (parse-statement-or-exp pre cur-tok getter)
    (let ((next (getter)))
      (if (id-token? (get-tok next))
          (parse-statement cur-tok next 'local getter #t)
          (parse-expression cur-tok next 'name getter))))
  
  ;parse-error: string position position
  (define (parse-error message start stop)
    (raise-read-error message
                      (file-path)
                      (position-line start)
                      (position-col start)
                      (+ (position-offset start) (interactions-offset))
                      (- (position-offset stop)
                         (position-offset start))))

  ;token = (list lex-token position position)
  (define (get-tok token) (car token))
  (define (get-start token) (cadr token))
  (define (get-end token) 
    (if (or (eq? (get-token-name (get-tok token)) 'STRING_LIT)
            (eq? (get-token-name (get-tok token)) 'STRING_ERROR))
        (cadr (token-value (get-tok token)))
        (caddr token)))
  
  ;output-format: token -> string
  (define (output-format tok)
    (cond
      ((separator? tok) 
       (case (get-token-name tok)
         ((O_BRACE) "{")
         ((C_BRACE) "}")
         ((O_PAREN) "(")
         ((C_PAREN) ")")
         ((O_BRACKET) "[")
         ((C_BRACKET) "]")
         ((SEMI_COLON) ";")
         ((COMMA) ",")
         ((PERIOD) ".")))
      ((eq? (get-token-name tok) 'OR) "operator ||")
      ((eq? (get-token-name tok) 'PIPE) "operator |")
      ((keyword? tok) (format "keyword ~a" (get-token-name tok)))
      ((id-token? tok) (format "identifier ~a" (token-value tok)))
      ((eq? (get-token-name tok) 'STRING_LIT) (format "string ~a" (car (token-value tok))))
      ((eq? (get-token-name tok) 'NULL_LIT) "null value")
      ((eq? (get-token-name tok) 'TRUE_LIT) "boolean value true")
      ((eq? (get-token-name tok) 'FALSE_LIT) "boolean value false")
      ((literal-token? tok) (format "value ~a" (token-value tok)))
      ((eq? (get-token-name tok) 'STRING_ERROR)
       (format "malformed string ~a" (car (token-value tok))))
      (else (get-token-name tok))))
  
  ;parse-definition: token token symbol (-> token) -> void
  (define (parse-definition pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (tokN (get-token-name tok))
           (srt (get-start cur-tok))
           (end (get-end cur-tok))
           (out (output-format tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
               
      (case state
        ((start) 
         (case tokN
           ((EOF) #t)
           ((class) (parse-definition cur-tok (getter) 'class-id getter))
           ((abstract) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((class? next-tok) (parse-definition cur-tok next state getter))
                ((eof? next-tok) (parse-error "abstract should be followed by class definition" srt end))
                (else 
                 (if (close-to-keyword? next-tok 'class)
                     (parse-error (format "expected 'class' after 'abstract,' found ~a which is incorrectly spelled or capitalized"
                                          (token-value next-tok))
                                  srt
                                  (get-end next))
                     (parse-error (format "abstract must be immediately followed by 'class' not ~a" (output-format next-tok))
                                  srt
                                  (get-end next)))))))
           ((interface) 
            (if (eq? (level) 'intermediate)
                (parse-definition cur-tok (getter) 'interface-id getter)
                (parse-error (format "Expected class definition, found ~a which may not be written here" out) srt end)))
           (else 
            (cond
              ((close-to-keyword? tok 'class)
                (parse-error (format "expected 'class', found ~a which is incorrectly spelled or capitalized"
                                     (token-value tok))
                             srt end))
              ((close-to-keyword? tok 'abstract)
               (parse-error (format "Expected 'abstract class' or 'class', found ~a which is incorrectly spelled or capitalized"
                                    (token-value tok))
                            srt end))
              ((and (eq? (level) 'intermediate) (close-to-keyword? tok 'interface))
               (parse-error (format "Expected 'interface' or 'class', found ~a which is incorrectly spelled or capitalized"
                                    (token-value tok)) srt end))
              ((or (if-token? tok) (return-token? tok))
               (parse-error (format "Expected class definition, found ~a. Statements must be in a method or interactions window" out)
                            srt end))
              ((prim-type? tok) 
               (parse-error (format "Expected class definition, found ~a. Fields and methods must be in a class body" out)
                            srt end))
              ((id-token? tok)
               (parse-error (format "Expected class definition, found ~a. Fields, methods, and expressions may not be written here"
                                    out) srt end))
              (else
               (parse-error (format "Expected class definition, found ~a which may not be written here" out)
                            srt end))))))
        ((class-id)
         (case tokN
           ((EOF) (parse-error "'class' should be followed by a class name and body" ps pe))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error (format "expected class body after ~a" (token-value tok)) srt end))
                ((extends? next-tok) (parse-definition next (getter) 'extends getter))
                ((and (eq? (level) 'intermediate) (implements? next-tok))
                 (parse-definition next (getter) 'implements getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((close-to-keyword? next-tok 'extends) 
                 (parse-error (format "found ~a, which is similar to 'extends'" (token-value next-tok))
                              (get-start next) (get-end next)))
                ((and (eq? (level) 'intermediate) (close-to-keyword? next-tok 'implements))
                 (parse-error (format "found ~a, which is similar to 'implements'" (token-value next-tok))
                              (get-start next) (get-end next)))
                ((open-separator? next-tok)
                 (parse-error (format "expected { to begin class body, but found ~a" (output-format next-tok))
                              (get-start next) (get-end next)))
                ((c-brace? tok)
                 (parse-error (format "Class body must be opened with { before being closed, found ~a" out)
                              (get-start next) (get-end next)))
                (else
                 (parse-error 
                  (format "class name must be followed by 'extends' or ~a a { to start class body, found ~a"
                          (if (eq? (level) 'intermediate) "'implements' clause or " "") 
                          (output-format next-tok)) srt (get-end next))))))
           (else 
            (if (keyword? tok) 
                (parse-error (format "class may not be called ~a as this is a reserved term" tokN) srt end)
                (parse-error (format "expected a name for this class, given ~a" out) srt end)))))
        ((interface-id)
         (case tokN
           ((EOF) (parse-error "'interface' should be followed by an interface name and body" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error (format "Expected interface body after ~a" (token-value tok)) srt end))
                ((extends? next-tok) (parse-definition cur-tok next 'iface-extends getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((close-to-keyword? next-tok 'extends)
                 (parse-error (format "found ~a, which is similar to 'extends'" (token-value next-tok)) 
                              (get-start next) (get-end next)))
                ((open-separator? next-tok)
                 (parse-error (format "Expected { to begin interface body, but found ~a" (output-format next-tok))
                              (get-start next) (get-end next)))
                ((c-brace? next-tok)
                 (parse-error (format "Interface body must be opened with { before being closed, found ~a"
                                      (output-format next-tok)) (get-start next) (get-end next)))
                ((implements? next-tok)
                 (parse-error "Interfaces may not implement other interfaces" ps (get-end next)))
                (else
                 (parse-error (format "Interface name must be follwed by 'extends' or a { to start its body, found ~a"
                                      (output-format next-tok)) srt (get-end next))))))
           (else
            (if (keyword? tok)
                (parse-error (format "interface may not be called ~a, as this is a reserved term" tokN) srt end)
                (parse-error (format "Expected a name for this interface, given ~a" out) srt end)))))
        ((extends) 
         (cond
           ((eof? tok) (parse-error "Expected parent class after extends" ps pe))
           ((id-token? tok)
            (if (not (eq? (level) 'intermediate))
                (parse-definition cur-tok (getter) 'class-body getter)
                (let* ((next (getter))
                       (next-tok (get-tok next)))
                  (cond
                    ((implements? next-tok) (parse-definition next (getter) 'implements getter))
                    ((close-to-keyword? next-tok 'implements)
                     (parse-error (format "Expected 'implements', found ~a which is close to 'implements'" (token-value next-tok))
                                  (get-start next) (get-end next)))
                    (else (parse-definition cur-tok next 'class-body getter))))))
           ((o-brace? tok) (parse-error "Expected a parent name after extends and before the class body starts" srt end))
           ((keyword? tok)
            (parse-error (format "parent may not be named after reserved word ~a" tokN) srt end))
           (else (parse-error (format "extends must be followed by parent name, found ~a" out) ps end))))
        ;Intermediate
        ((implements)
         (cond
           ((eof? tok) (parse-error "Expected implemented interface after implements, and class body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more implemented interfaces or class body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'implements-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((id-token? next-tok)
                 (parse-error "Implemented interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the class body, found ~a" (output-format next-tok))
                                   (get-start next) (get-end next))))))
           ((keyword? tok)
            (parse-error (format "Expected an interface name, which may not be reserved word ~a" tokN) srt end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end))))
        ;Intermediate
        ((implements-list)
         (cond
           ((eof? tok) (parse-error "Expected an interface name and class body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more interfaces or a class body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'implements-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'class-body getter))
                ((id-token? next-tok) (parse-error "Implemented interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the class body, found ~a" (output-format next-tok))
                                   (get-start next) (get-end next))))))
           ((keyword? tok)
            (parse-error (format "Expected an interface name for implements clause, found reserved term ~a" tokN) srt end))
           ((o-brace? tok)
            (parse-error "Expected an additional interface after comma before { to start class body" ps end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end))))
        ;Intermediate 
        ((iface-extends)
         (cond
           ((eof? tok) (parse-error "Expected interface name to extend after extends, and interface body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more extended interfaces or interface body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'iface-extends-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((implements? next-tok) 
                 (parse-error "An interface may not implement other interfaces" (get-start next) (get-end next)))
                ((id-token? next-tok) (parse-error "Extended interfaces must be separated by a comma" srt (get-end next)))
                (else 
                 (parse-error (format "Expected more interfaces to extend of interface body, found ~a" (output-format next-tok))
                              (get-start next) (get-end next))))))
           ((keyword? tok)
            (parse-error (format "Expected a name of an interface to extend, found reserved term ~a, which cannot be a name"
                                 tokN) srt end))
           (else
            (parse-error (format "Expected a name of an interface to extend, found ~a" out) srt end))))
        ;Intermediate
        ((iface-extends-list)
         (cond
           ((eof? tok) (parse-error "Expected interface name to extend after comma, and interface body" ps pe))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected more interfaces or an interface body" srt end))
                ((comma? next-tok) (parse-definition next (getter) 'iface-extends-list getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'iface-body getter))
                ((id-token? next-tok) (parse-error "Extended interfaces must be separated by a comma" srt (get-end next)))
                (else (parse-error (format "Expected more interfaces or the interface body, found ~a" (output-format next-tok))
                                   (get-start next) (get-end next))))))
           ((keyword? tok)
            (parse-error (format "Expected an interface name for extends clause, found reserved term ~a" tokN) srt end))
           ((o-brace? tok)
            (parse-error "Expected an additional interface after comma before { to start interface body" ps end))
           (else (parse-error (format "Expected an interface name, found ~a" out) srt end)))) 
        ((class-body)
         (case tokN
           ((EOF) (parse-error (format "Expected class body to begin after ~a" (output-format (get-tok pre))) ps pe))
           ((O_BRACE) (parse-definition cur-tok (parse-members null (getter) 'start getter) 'class-body-end getter))
           (else 
            (cond
              ((open-separator? tok)
               (parse-error (format "expected { to begin class body, but found ~a" out) srt end))
              ((close-separator? tok)
               (parse-error (format "Class body must be opened with { before being closed, found ~a" out) srt end))
              (else
               (parse-error (format "Expected { to start class body, found ~a" out) srt end))))))
        ((class-body-end)
         (case tokN
           ((EOF) (parse-error "Expected a } to close class body" ps pe))
           ((C_BRACE) 
            (let ((next (getter)))
              (if (c-brace? (get-tok next))
                  (parse-error "Unnecessary }, class body already closed" srt (get-end next))
                  (parse-definition cur-tok next 'start getter))))
           (else (parse-error (format "Expected a } to close class body, found ~a" out) ps end))))
        ((iface-body)
         (case tokN
           ((EOF) (parse-error (format "Expected interface body to begin after ~a" (output-format (get-tok pre))) ps pe))
           ((O_BRACE) (parse-definition cur-tok (parse-iface-body null (getter) 'start getter) 'iface-body-end getter))
           (else
            (cond
              ((open-separator? tok)
               (parse-error (format "Expected { to begne interface body, but found ~a" out) srt end))
              ((close-separator? tok)
               (parse-error (format "Interface body must be opened with { before being closed, found ~a" out) srt end))
              (else (parse-error (format "Expected { to start interface body, found ~a" out) srt end))))))
        ((iface-body-end)
          (case tokN
            ((EOF) (parse-error "Expected a } to close interface body" ps pe))
            ((C_BRACE)
             (let ((next (getter)))
               (if (c-brace? (get-tok next))
                   (parse-error "Unnecessary }, interface body is already closed" srt (get-end next))
                   (parse-definition cur-tok next 'start getter))))
            (else (parse-error (format "Expected a } to close interface body, found ~a" out) ps end)))))))
               
              
  ;parse-members: token token symbol (->token) -> token
  (define (parse-members pre cur state getter) 
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (output-format tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))

      (case state
        ((start)
         (cond
           ((or (eof? tok) (c-brace? tok)) cur)
           ((abstract? tok) (parse-members cur (getter) 'method getter))
           ((prim-type? tok) (parse-members cur (getter) 'method-or-field getter))
           ((and (eq? (level) 'intermediate) (void-token? tok)) (parse-members cur (getter) 'method-id getter))
           ((id-token? tok) (parse-members cur (getter) 'member getter))
           (else 
            (parse-error 
             (format "Only fields, methods and a constructor may be within the class body, found ~a" out) srt end))))
        ((member)
         (cond
           ((eof? tok) (parse-error "Class member may not end here, class body still requires a }" ps pe))
           ((dot? tok) (parse-members cur (parse-name (getter) getter) 'method-or-field getter))
           ((id-token? tok) (parse-members pre cur 'method-or-field getter))
           ((o-paren? tok) (parse-members cur (getter) 'ctor-parms getter))
           ((c-paren? tok) (parse-error "( must precede ) in parameter list" srt end))
           ((open-separator? tok) 
            (parse-error (format "( must be used to start parameter list, found ~a" out) srt end))
           ((prim-type? tok)
            (parse-error 
             (format "methods and fields may not be named for primitive type ~a, which appears in the name position" kind)
             srt end))
           ((keyword? tok)
            (parse-error 
             (format "Expected a name for this field or method, ~a is a reserved word and cannot be the name" kind)
             srt end))
           (else (parse-error (format "Expected a name for this field or method, found ~a" out) srt end))))
        ((method-or-field)
         (case kind
           ((EOF) (parse-error "Method or field must have a name, class body still requires a }" ps pe))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (output-format n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) 
                 (parse-error "Method or field has not completed, class body still requires a }" srt end))
                ;Just ended a field
                ((semi-colon? n-tok) (parse-members next (getter) 'start getter))
                ((comma? n-tok) 
                 (if (eq? (level) 'intermediate)
                     (parse-members next (getter) 'field-list getter)
                     (parse-error (format "Expected an end to field ~a, fields end in ';', ',' is not allowed" (token-value tok))
                                  srt ne)))
                ((and (eq? (level) 'intermediate) (teaching-assignment-operator? n-tok))
                 (let ((assign-exp (getter)))
                   (if (eof? (get-tok assign-exp))
                       (parse-error (format "Expected an expression to bind to ~a, and class body still needs a }" 
                                            (token-value tok)) srt end)
                       (parse-members next (parse-expression null assign-exp 'start getter) 'field-init-end getter))))
                ((o-paren? n-tok) (parse-members next (getter) 'method-parms getter))
                ((open-separator? n-tok) 
                 (parse-error (format "Method parameters must begin with ( found ~a" n-out) srt ne))
                ((id-token? n-tok)
                 (parse-error
                  (if (eq? (level) 'intermediate)
                      (format "Fields must be separated by commas, method paramters must be in ()s, ~a not allowed" n-out)
                      (format "Fields must be separatley declared, method paramters must be in ()s, ~a not allowed" n-out))
                  srt ne))                
                (else (parse-error 
                       (format "Expected ; to end field or method parameter list, found ~a" n-out) srt ne)))))
           (else 
            (parse-error 
             (if (keyword? tok)
                 (format "Expected a name for this field or method, cannot be named reserved word ~a" kind)
                 (format "Expected a name for this field or method, found ~a" out))
             srt end))))
        ;Intermediate
        ((field-list)
         (case kind
           ((EOF) (parse-error "Expected an additional field name after comma, class body still requires a }" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (output-format n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Field has not completed, class body still requires a }" srt end))
                ((semi-colon? n-tok) (parse-members next (getter) 'start getter))
                ((comma? n-tok) (parse-members next (getter) 'field-list getter))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (if (eof? (get-tok assign-exp))
                       (parse-error (format "Expected an expression to bind to ~a, and class body still needs a }" 
                                            (token-value tok)) srt end)
                       (parse-members next (parse-expression null assign-exp 'start getter) 'field-init-end getter))))
                ((id-token? n-tok)
                 (parse-error (format "Fields must be separated by commas, ~a not allowed" n-out) srt ne))
                (else (parse-error (format "Expected ; to end field, or more field names, found ~a" n-out) srt ne)))))
           (else
            (parse-error
             (if (keyword? tok)
                 (format "Expected a name for this field, cannot be named reseved word ~a" kind)
                 (format "Expected a name for this field, found ~a" out)) srt end))))
        ;Intermediate
        ((field-init-end)
         (case kind
           ((EOF) (parse-error "Expected a ; or comma after field, class body still requires a }" ps pe))
           ((COMMA) (parse-definition cur (getter) 'field-list getter))
           ((SEMI_COLON) (parse-definition cur (getter) 'start getter))
           ((IDENTIFIER) (parse-error (format "Fields must be separated by commas, ~a not allowed" out) srt end))
           (else (parse-error (format "Expected a ; to end field, or more field names, found ~a" out) srt end))))                
        ((method)
         (cond
           ((eof? tok) (parse-error "Expected method, and class body still requires a }" ps pe))
           ((or (prim-type? tok) (and (eq? (level) 'intermediate) (void-token? tok)))
            (parse-members cur (getter) 'method-id getter))
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-kind (get-token-name next-tok))
                   (next-end (get-end next))
                   (next-start (get-start next)))
              (cond
                ((eof? next-tok) (parse-error "Expected method name, and class body still requires a }" srt end))
                ((dot? next-tok) (parse-members next (parse-name (getter) getter) 'method-id getter))
                ((o-paren? next-tok) 
                 (parse-error "Declaration is similar to constructor, which cannot be abstract" ps next-end))
                ((semi-colon? next-tok) 
                 (parse-error "Declaration is similar to a field, which cannot be abstract" ps next-end))
                ((id-token? next-tok) (parse-members cur next 'method-id getter))
                ((keyword? next-tok) 
                 (parse-error 
                  (format "Expected method name, found ~a which is reserved and cannot be a method's name" next-kind)
                  next-start next-end))
                (else (parse-error (format "Expected a method name, found ~a" (output-format next-tok)) 
                                   next-start next-end)))))
           ((keyword? tok)
            (parse-error 
             (format "Expected return type of the method, reserved word ~a is not a type" kind) srt end))
           (else (parse-error (format "Expected return type of a method, found ~a" out) srt end))))
        ((method-id)
         (case kind
           ((EOF) (parse-error "Expected method name, and class body still requires a }" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (output-format next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) (parse-error "Expected method body, and class body still requires a }" srt end))
                ((o-paren? next-tok) (parse-members next (getter) 'method-parms getter))
                ((c-paren? next-tok) 
                 (parse-error "Expected a ( to start parameter list but encountered the closing )" next-start next-end))
                ((semi-colon? next-tok) 
                 (parse-error "Declaration is similar to a field, which cannot be abstract" ps next-end))
                ((open-separator? next-tok)
                 (parse-error (format "Method parameter list is started by (, found ~a" next-out) next-start next-end))
                (else (parse-error (format "Expected ( for parameter list, found ~a" next-out) next-start next-end)))))
           (else
            (if (keyword? tok)
                (parse-error 
                 (format "Expected method name, found ~a which is reserved and cannot be a method's name" kind)
                 srt end)
                (parse-error (format "Expected method name, found ~a" out) srt end)))))
        ((ctor-parms)
         (cond
           ((eof? tok) (parse-error "Expected constructor parameters, and class body still requires a }" ps pe))
           ((o-paren? tok) 
            (parse-error "Constructor parameter list already started, an additional ( is not needed" srt end))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (output-format next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))                             
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected constructor body, and class body still requires a }" srt end))
                ((c-paren? next-tok) 
                 (parse-error "Constructor parameter list already closed, unneeded )" next-start next-end))
                ((o-brace? next-tok) 
                 (parse-members next 
                                (if (eq? (level) 'intermediate)
                                    (parse-intermediate-ctor-body null (getter) getter)
                                    (parse-beginner-ctor-body null (getter) 'start getter))
                                'ctor-end getter))
                ((open-separator? next-tok)
                 (parse-error (format "Constructor body begins with a {, found ~a" next-out) next-start next-end))
                ((semi-colon? next-tok) 
                 (parse-error "Expected a constructor body, ; is only allowed for abstract methods" next-start next-end))
                (else
                 (parse-error ("Expected a constructor body, starting with {, found ~a" next-out) next-start next-end)))))
           ((or (prim-type? tok) (id-token? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of parameter list, and class body still requires a }"  srt end))
                ((comma? next-tok) (parse-error "Variable name must follow type before ," srt next-end))
                ((c-paren? next-tok) (parse-error "Variable name must follow type before )" srt next-end))
                ((id-token? next-tok)
                 (let* ((afterID (getter))
                        (afterID-tok (get-tok afterID)))
                   (cond
                     ((eof? afterID-tok) 
                      (parse-error "Expected rest of parameter list, and class body requires a }" next-start next-end))
                     ((c-paren? afterID-tok) (parse-members next afterID 'ctor-parms getter))
                     ((close-separator? afterID-tok) 
                      (parse-error (format "Expected a ) to close parameter list, found ~a" (output-format afterID-tok))
                                   (get-start afterID) (get-end afterID)))
                     ((comma? afterID-tok)
                      (let* ((afterC (getter))
                             (afterC-tok (get-tok afterC)))
                        (cond
                          ((eof? afterC-tok) (parse-error "Expected rest of parameter list, and class body requires a }"
                                                          (get-start afterID) (get-end afterID)))
                          ((c-paren? afterC-tok)
                           (parse-error "Comma is unneeded before ) unless another variable is desired" 
                                        (get-start afterID) (get-end afterC)))
                          ((comma? afterC-tok)
                           (parse-error "Parameter list should not have ,, Only one is needed" 
                                        (get-start afterID) (get-end afterC)))
                          (else (parse-members afterID afterC 'ctor-parms getter)))))
                     ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                      (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                           (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                   next-start (get-end afterID)))
                     (else (parse-error (format "Expected , or ) in parameter list found ~a" (output-format afterID-tok))
                                        (get-start afterID) (get-end afterID))))))
                ((keyword? next-tok)
                 (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                      (get-token-name next-tok))
                              next-start next-end))
                (else (parse-error (format "Expected new parameter name after type, found ~a" (output-format next-tok))
                                   next-start next-end)))))
           ((keyword? tok)
            (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
           (else (parse-error (format "Expected a parameter or ), found ~a" out) srt end))))
        ((method-parms)
         (cond
           ((eof? tok) (parse-error "Expceted method parameters, and class body still requires }" ps pe))
           ((o-paren? tok) 
            (parse-error "Method parameter list already started, an additional ( is not needed" srt end))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-out (output-format next-tok))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok)
                 (parse-error "Expected method body, and class body still requires a }" srt end))
                ((c-paren? next-tok) 
                 (parse-error "Method parameter list already closed, unneeded )" next-start next-end))
                ((o-brace? next-tok)
                 (parse-members next (if (eq? (level) 'intermediate)
                                         (parse-method-body null (getter) getter)
                                         (parse-statement null (getter) 'start getter #f))
                                'method-end getter))                
                ((open-separator? next-tok)
                 (parse-error (format "Method body begins with a {, found ~a" next-out) next-start next-end))
                ((semi-colon? next-tok) (parse-members next (getter) 'start getter))
                (else
                 (parse-error ("Expected a method body, starting with {, found ~a" next-out) next-start next-end)))))
           ((or (prim-type? tok) (id-token? tok))
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (next-start (get-start next))
                   (next-end (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of parameter list, and class body still requires a }" srt end))
                ((comma? next-tok)  (parse-error "Variable name must follow type before ," srt next-end))
                ((c-paren? next-tok)  (parse-error "Variable name must follow type before )" srt next-end))
                ((id-token? next-tok)
                 (let* ((afterID (getter))
                        (afterID-tok (get-tok afterID)))
                   (cond
                     ((eof? afterID-tok) 
                      (parse-error "Expected rest of parameter list, and class body requires a }" next-start next-end))
                     ((c-paren? afterID-tok) (parse-members next afterID 'method-parms getter))
                     ((comma? afterID-tok)
                      (let* ((afterC (getter))
                             (afterC-tok (get-tok afterC)))
                        (cond
                          ((eof? afterC-tok) (parse-error "Expected rest of parameter list, and class body requires a }"
                                                          (get-start afterID) (get-end afterID)))
                          ((c-paren? afterC-tok)
                           (parse-error "Comma is unneeded before ) unless another variable is desired" 
                                        (get-start afterID) (get-end afterC)))
                          ((comma? afterC-tok) 
                           (parse-error "Parameter list should not have ,, Only one is needed" 
                                        (get-start afterID) (get-end afterC)))
                          (else (parse-members afterID afterC 'method-parms getter)))))
                     ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                      (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                           (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                   next-start (get-end afterID)))
                     (else (parse-error (format "Expected , or ) in parameter list found ~a" (output-format afterID-tok))
                                        (get-start afterID) (get-end afterID))))))
                ((keyword? next-tok)
                 (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                      (get-token-name next-tok))
                              next-start next-end))
                (else (parse-error (format "Expected new parameter name after type, found ~a" (output-format next-tok))
                                   next-start next-end)))))
           ((keyword? tok)
            (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
           (else (parse-error (format "Expected a parameter or ), found ~a" (output-format tok)) srt end))))
        ((ctor-end)
         (case kind
           ((EOF) (parse-error "Expected } to end constructor body, and class body still requires }" ps pe))
           ((C_BRACE) (parse-members cur (getter) 'start getter))
           ((if return) 
            (parse-error (format "Statements are not permitted in the constructor body, found ~a" kind) srt end))
           (else (parse-error (format "Expected a } to end the constructor, found ~a" out) srt end))))
        ((method-end)
         (case kind
           ((EOF) (parse-error "Expected } to end method body, and class body still requires }" ps pe))
           ((C_BRACE) (parse-members cur (getter) 'start getter))
           (else 
            (parse-error (format "Expected 1 statement, and then } for method body. Found ~a instead of }" out)
                         srt end)))))))

  ;Beginner
  ;parse-field: token token symbol (->token) -> token
  (define (parse-field pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (output-format tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((EOF) (parse-error "Expected a name for this variable declaration" ps pe))
           ((IDENTIFIER) (parse-field cur-tok (getter) 'equals getter))
           ((=) (parse-error "Expected a name for this variable declaration inbetween the type and =" ps end))
           (else
            (if (keyword? tok)
                (parse-error 
                 (format "Expected a name for this declaration, reserved word ~a may not be the name" kind) start end)
                (parse-error
                 (format "Expected a name for this declaration, found ~a" out) start end)))))
        ((equals)
         (case kind
           ((=) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression for this declaration" start end)
                  (parse-field cur-tok (parse-expression cur-tok next 'start getter) 'end getter))))
           ((COMMA)
            (parse-error "Expected an assignment of the given name to a value, found ',' which is not allowed here."
                         ps end))
           ((SEMI_COLON)
            (parse-error "Expected an assignment of the given name to a value, found ';' which is not allowed here."
                         ps end))
           (else
            (parse-error (format "Expected an assignment of the given name to a value, found ~a" out) ps end))))
        ((end)
         (case kind
           ((EOF) (parse-error "Declaration must end with a ';'" ps pe))
           ((SEMI_COLON) (getter))
           (else
            (parse-error (format "Expected an end to this declartion, found ~a" out) start end)))))))
  ;Intermediate
  ;parse-iface-body: token token symbol (->token) -> token
  (define (parse-iface-body pre cur state getter) 
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (output-format tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
    (case state
      ((start)          
       (cond
         ((or (eof? tok) (c-brace? tok)) cur)
         ((abstract? tok) (parse-iface-body cur (getter) 'method-type getter))
         ((prim-type? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((void-token? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((id-token? tok) (parse-iface-body cur (getter) 'method-id getter))
         (else 
          (parse-error 
           (format "Only methods may be within the interface body, found ~a" out) srt end))))
      ((method-type)
       (cond
         ((eof? tok) (parse-error "Expected method, and interface body still requires a }" ps pe))
         ((or (prim-type? tok) (void-token? tok)) (parse-iface-body cur (getter) 'method-id getter))
         ((id-token? tok) (parse-iface-body cur (getter) 'method-id getter))
         ((keyword? tok)
          (parse-error 
           (format "Expected return type of the method, reserved word ~a is not a type" kind) srt end))
         (else (parse-error (format "Expected return type of a method, found ~a" out) srt end))))
      ((method-id)
       (case kind
         ((EOF) (parse-error "Expected method name, and interface body still requires a }" ps pe))
         ((IDENTIFIER)
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-out (output-format next-tok))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) (parse-error "Expected method parameters, and interface body still requires a }" srt end))
              ((o-paren? next-tok) (parse-iface-body next (getter) 'parms getter))
              ((c-paren? next-tok)
               (parse-error "Expected a ( to start parameter list but encountered the closing )" next-start next-end))
              ((semi-colon? next-tok) 
               (parse-error "Declaration is similar to a field, which maynot be in an interface" ps next-end))
              ((open-separator? next-tok)
               (parse-error (format "Method parameter list is started by (, found ~a" next-out) next-start next-end))
              (else (parse-error (format "Expected ( for parameter list, found ~a" next-out) next-start next-end)))))
         ((PERIOD) 
          (if (id-token? (get-tok pre)) 
              (parse-iface-body cur (parse-name (getter) getter) 'method-id getter)
              (parse-error (format "~a cannot be followed by '.'" (output-format pre)) ps end)))
         ((O_PAREN) (parse-error "Expected a method name before parameter list" ps end))
         ((SEMI_COLON) (parse-error "Declaration is similar to a field, which maynot be in an interface" ps end))
         (else
          (if (keyword? tok)
              (parse-error 
               (format "Expected method name, found ~a which is reserved and cannot be a method's name" kind) srt end)
              (parse-error (format "Expected method name, found ~a" out) srt end)))))
      ((parms)
       (cond
         ((eof? tok) (parse-error "Expceted method parameters, and interface body still requires }" ps pe))
         ((o-paren? tok) 
          (parse-error "Method parameter list already started, an additional ( is not needed" srt end))
         ((c-paren? tok)
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-out (output-format next-tok))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) (parse-error "Expected a ';', and interface body still requires a }" srt end))
              ((c-paren? next-tok) 
               (parse-error "Method parameter list already closed, unneeded )" next-start next-end))
              ((o-brace? next-tok)
               (parse-error "Method in interface maynot have a body" next-start next-end))
              ((semi-colon? next-tok) (parse-iface-body next (getter) 'start getter))
              (else
               (parse-error (format "Expected a ';', found ~a" next-out) next-start next-end)))))
         ((or (prim-type? tok) (id-token? tok))
          (let* ((next (getter))
                 (next-tok (get-tok next))
                 (next-start (get-start next))
                 (next-end (get-end next)))
            (cond
              ((eof? next-tok) 
               (parse-error "Expected rest of parameter list, and interface body still requires a }" srt end))
              ((comma? next-tok)  (parse-error "Variable name must follow type before ," srt next-end))
              ((c-paren? next-tok)  (parse-error "Variable name must follow type before )" srt next-end))
              ((id-token? next-tok)
               (let* ((afterID (getter))
                      (afterID-tok (get-tok afterID))
                      (afterID-s (get-start afterID)))
                 (cond
                   ((eof? afterID-tok) 
                    (parse-error "Expected rest of parameter list, and interface body requires a }" next-start next-end))
                   ((c-paren? afterID-tok) (parse-iface-body next afterID 'parms getter))
                   ((comma? afterID-tok)
                    (let* ((afterC (getter))
                           (afterC-tok (get-tok afterC))
                           (afterC-end (get-end afterC)))
                      (cond
                        ((eof? afterC-tok) 
                         (parse-error "Expected rest of parameter list, and class body requires a }" afterID-s afterC-end))
                        ((c-paren? afterC-tok)
                         (parse-error "Comma is unneeded before ) unless another variable is desired" afterID-s afterC-end))
                        ((comma? afterC-tok) 
                         (parse-error "Parameter list should not have ',,' Only one is needed" afterID-s afterC-end))
                        (else (parse-iface-body afterID afterC 'parms getter)))))
                   ((or (prim-type? afterID-tok) (id-token? afterID-tok))
                    (parse-error (format "~a begins a new parameter. A , is needed in between parameters"
                                         (if (prim-type? afterID-tok) (get-token-name afterID-tok) (token-value afterID-tok)))
                                 next-start (get-end afterID)))
                   (else (parse-error (format "Expected , or ) in parameter list found ~a" (output-format afterID-tok))
                                      afterID-s (get-end afterID))))))
              ((keyword? next-tok)
               (parse-error (format "Expected variable name after type, found reserved word ~a, which cannot be a name"
                                    (get-token-name next-tok))
                            next-start next-end))
              (else (parse-error (format "Expected new parameter name after type, found ~a" (output-format next-tok))
                                 next-start next-end)))))
         ((keyword? tok)
          (parse-error (format "Expected type name, reserved word ~a is not a type" kind) srt end))
         (else (parse-error (format "Expected a parameter or ), found ~a" (output-format tok)) srt end)))))))
  
  ;parse-type: token (-> token) -> token
  (define (parse-name cur-tok getter) 
    (let* ((tok (car cur-tok))
           (kind (get-token-name tok))
           (start (cadr cur-tok))
           (stop (caddr cur-tok)))
      (case kind
        ((IDENTIFIER) 
         (let ((next-tok (getter)))
           (if (dot? (car next-tok))
               (parse-name (getter) getter)
               next-tok)))
        ((PERIOD) (parse-error "It is not allowed to have two .s, only one is necessary" start stop))
        (else 
         (if (keyword? tok)
             (parse-error (format "Expected variable after ., found reserved word ~a, which may not be a variable" kind)
                          start stop)
             (parse-error (format "Expected variable after . in name, found ~a" (output-format tok)) start stop))))))
  
  ;Intermediate
  ;parse-intermediate-ctor-body: token token (->token) -> token
  (define (parse-intermediate-ctor-body pre cur-tok getter)
    (case (get-token-name (get-tok cur-tok))
      ((EOF C_BRACE) cur-tok)
      ((super this) 
       (parse-method-body pre (parse-ctor-call cur-tok (getter) 'start getter) getter))
      (else (parse-method-body pre cur-tok getter))))

  ;Intermediate
  ;parse-ctor-call: token token symbol 
  (define (parse-ctor-call pre cur-tok state getter) 
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (output-format tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((O_PAREN) (parse-ctor-call cur-tok (getter) 'ctor-args getter))
           ((PERIOD) 
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (ne (get-end next)))
              (cond
                ((id-token? next-tok) (parse-statement next (getter) 'assign-or-call getter #t))
                ((keyword? next-tok)
                 (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name next-tok))
                              start ne))
                (else
                 (parse-error (format "Expected identifer after '.', found ~a" (output-format next-tok)) start ne)))))
           (else (parse-error (format "~a cannot be used here" (get-token-name pre)) ps end))))
        ((ctor-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((c-paren? tok)
            (let ((next (getter)))
              (if (semi-colon? (get-tok next))
                  (parse-method-body next (getter) getter)
                  (parse-error (format "Expected a ';' after constructor call, found ~a" (output-format (get-tok next)))
                               start (get-end next)))))
           (else (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter) 'more-args getter))))
        ((more-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((C_PAREN) (parse-ctor-call pre cur-tok 'ctor-args getter))
           ((COMMA) 
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-ctor-call cur-tok (parse-expression cur-tok next 'start getter) 'more-args getter))))
           (else 
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close constructor arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between constructor arguments, found ~a" out) start end))))))))

  
  ;Beginner
  ;parse-beginner-ctor-body: token token symbol (-> token) -> token
  (define (parse-beginner-ctor-body pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (output-format tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((C_BRACE EOF) cur-tok)
           ((this) 
            (let* ((next (getter))
                   (next-tok (get-tok next))
                   (ns (get-start next))
                   (ne (get-end next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of field initialization, constructor and class need }" start end))
                ((dot? next-tok)
                 (let* ((afterD (getter))
                        (afterD-tok (get-tok afterD))
                        (ae (get-end afterD)))
                   (cond
                     ((id-token? afterD-tok) (parse-beginner-ctor-body afterD (getter) 'assign-op getter))
                     ((keyword? afterD-tok) 
                      (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name afterD-tok))
                                   ns ae))
                     (else
                      (parse-error (format "Expected identifer after '.', found ~a" (output-format afterD-tok)) ns ae)))))
                (else (parse-error (format "Expected this.Field, found ~a instead of '.'" (output-format next-tok)) ns ne)))))
           ((IDENTIFIER)
            (let ((next (getter)))
              (if (dot? (get-tok next)) 
                  (parse-beginner-ctor-body next (parse-name (getter) getter) 'assign-op getter)
                  (parse-beginner-ctor-body cur-tok next 'assign-op getter))))
           (else 
            (if (keyword? tok)
                (parse-error (format "Expected name, found reserved word ~a" kind) start end)
                (parse-error (format "Expected name, found ~a" out) start end)))))
        ((assign-op)
         (case kind
           ((EOF) (parse-error "Expected rest of field initialization (=), constructor and class need }" ps pe))
           ((=) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after = for field initialization" start end)
                  (parse-beginner-ctor-body cur-tok (parse-expression null next 'start getter) 'assign-end getter))))
           (else (parse-error (format "Expected = to be used in initializing the field, found ~a" out) start end))))
        ((assign-end)
         (cond
           ((eof? tok) (parse-error "Expected a ; to end field intialization, constructor and class need }" ps pe))
           ((semi-colon? tok) (parse-beginner-ctor-body cur-tok (getter) 'start getter))
           (else (parse-error (format "Expected a ; to end field initialization, found ~a" out) start end)))))))
  
    
  ;Intermediate
  ;parse-method-body: token token (->token) -> token
  (define (parse-method-body pre cur-tok getter)
    (case (get-token-name (get-tok cur-tok))
      ((C_BRACE EOF) cur-tok)
      (else (parse-method-body pre (parse-statement pre cur-tok 'start getter #t) getter))))
  
  ;Intermediate - addition of parameter id-ok?
  ;parse-statement: token token symbol (->token) bool -> token
  (define (parse-statement pre cur-tok state getter id-ok?)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (output-format tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
      (case state
        ((start)
         (case kind
           ((if) (parse-statement cur-tok (getter) 'if getter id-ok?))
           ((return)
            (let ((next (getter)))
              (cond
                ((eof? (get-tok next)) 
                 (parse-error (if (eq? (level) 'intermediate) 
                                  "Expected rest of return" 
                                  "Expected expression for return") 
                              start end))
                ((and (eq? (level) 'intermediate) (semi-colon? (get-tok next))) (getter))
                (else (parse-statement cur-tok (parse-expression null next 'start getter) 'return getter id-ok?)))))
           ((IDENTIFIER)
            (if (not (eq? (level) 'intermediate))
                (let ((v (token-value tok)))
                  (cond
                    ((close-to-keyword? tok 'if) 
                     (parse-error (format "Expected if, found ~a which is perhaps miscapitalized or spelled" v) start end))
                    ((close-to-keyword? tok 'return)
                     (parse-error (format "Expected return, found ~a which is perhaps miscapitalized or spelled" v) start end))
                    (else
                     (parse-error (format "Expected a statement, found ~a. Statements begin with 'if' or 'return'" out) 
                                  start end))))                
                (let ((next (getter)))
                  (if (dot? (get-tok next)) 
                      (parse-statement next (parse-name (getter) getter) 'statement-or-var getter id-ok?)
                      (parse-statement cur-tok next 'statement-or-var getter id-ok?)))))
           (else
            (when (eq? (level) 'beginner)
              (parse-error (format "Expected a statement, found ~a. Statements begin with 'if' or 'return'" out) start end))
            ;Intermediate cases
            (case kind
              ;From ctor-beginner-body
              ((this super)
               (let* ((next (getter))
                      (next-tok (get-tok next))
                      (ns (get-start next))
                      (ne (get-end next)))
                 (cond
                   ;Intermediate error change
                   ((eof? next-tok) (parse-error (format "Expected ~a.name, unexpected end" kind) start end))
                   ((dot? next-tok)
                    (let* ((afterD (getter))
                           (afterD-tok (get-tok afterD))
                           (ae (get-end afterD)))
                      (cond
                        ;Intermediate changed next state
                        ((id-token? afterD-tok) (parse-statement afterD (getter) 'assign-or-call getter id-ok?))
                        ((keyword? afterD-tok)
                         (parse-error (format "Expected identifier after '.', found reserved word ~a" (get-token-name afterD-tok))
                                      ns ae))
                        (else
                         (parse-error (format "Expected identifer after '.', found ~a" (output-format afterD-tok)) ns ae)))))
                   (else (parse-error (format "Expected ~a.name, found ~a instead of '.'" kind (output-format next-tok)) ns ne)))))           
              ;Intermediate
              ((new) (parse-statement cur-tok (parse-expression cur-tok (getter) 'class-alloc-start getter) 'end-exp getter id-ok?))
              ;Intermediate
              ((O_PAREN) 
               (parse-statement cur-tok 
                                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter) 'c-paren getter)
                                'end-exp getter id-ok?))
              ;Intermediate
              ((O_BRACE) (parse-statement cur-tok (parse-method-body cur-tok (getter) getter) 'c-brace getter #t))
              ;Intermediate - changed wholly
              (else
               (cond
                 ((prim-type? tok) (parse-statement cur-tok (getter) 'local getter id-ok?))
                 ((keyword? tok)
                  (parse-error (format "Expected name, found reserved word ~a" kind) start end))
                 (else (parse-error (format "Expected statement, found ~a, which cannot begin a statement" out) start end))))))))
        ((if)
         (case kind
           ((EOF) (parse-error "Expected conditional test for if" ps pe))
           ((O_PAREN) 
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error (format "Expected conditional expression for if") start end)
                  (parse-statement cur-tok (parse-expression null next 'start getter) 'if-then getter id-ok?))))
           (else 
            (parse-error (format "Conditional expression for if must be started with (, found ~a" out) start end))))
        ((if-then)
         (case kind
           ((EOF) (parse-error "Expected ) to close conditional for if, and then and else statements for if" ps pe))
           ((C_PAREN) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected statement for then branch of if" start end))
                ((c-paren? next-tok) (parse-error "Conditional expression already closed, extra ) found" start (get-end next)))
                (else (parse-statement cur-tok (parse-statement null next 'start getter #f) 'if-else getter id-ok?)))))
           (else 
            (parse-error (format "Conditional expression for if must be in parens, did not find ), found ~a" out) ps end))))
        ((if-else)
         (case kind
           ((EOF) (parse-error "Expected else for if statement" ps pe))
           ((else) (parse-statement null (getter) 'start getter #f))
           (else
            (parse-error
             (if (and (id-token? tok) (close-to-keyword? tok 'else))
                 (format "Expected 'else' for if, found ~a, which might be mispelled or miscapitalized" (token-value tok))
                 (format "Expected 'else for if, found ~a" out))
             start end))))
        ((return)
         (case kind
           ((EOF) (parse-error "Expected ; to end return statement" ps pe))
           ((SEMI_COLON) (getter))
           (else (parse-error (format "Expected ; to end return statement, found ~a" out) start end))))
        ;Intermediate
        ((statement-or-var)
         (case kind
           ((EOF) (parse-error "Expected remainder of statement" ps pe))
           ((IDENTIFIER) (parse-statement pre cur-tok 'local getter id-ok?))
           (else (parse-statement pre cur-tok 'assign-or-call getter id-ok?))))
        ;Intermediate
        ((assign-or-call)
         (case kind
           ((EOF) (parse-error "Expected remainder of assignment or call" ps pe))
           ((=)
            ;From Assignment
            (let ((next (getter)))
              (if (eof? (get-tok next))
                  (parse-error "Expected an expression after = for assignment" start end)
                  (parse-statement cur-tok (parse-expression null next 'start getter) 'assign-end getter id-ok?))))
           ((O_PAREN) (parse-statement cur-tok (parse-expression pre cur-tok 'method-call-args getter) 'exn-end getter id-ok?))
           (else (parse-error (format "Expected assignment or method call, found ~a, which is not valid for a statement" out)
                              start end))))
        ;Intermediate - from Assignment, error messages changed
        ((assign-end)
         (cond
           ((eof? tok) (parse-error "Expected a ; to end assignment" ps pe))
           ((semi-colon? tok) (getter))
           (else (parse-error (format "Expected a ; to end field initialization, found ~a" out) start end))))
        ;Intermediate
        ((end-exp)
         (case kind
           ((EOF) (parse-error "Expected ';' or rest of Statement" ps pe))
           ((PERIOD)
            (let ((next (getter)))
              (cond
                ((id-token? (get-tok next)) (parse-statement next (getter) 'assign-or-call getter id-ok?))
                (else 
                 (parse-error (format "Expected a name after '.', found ~a" (output-format (get-tok next)))
                              ps (get-end next))))))
           ((SEMI_COLON) (getter))
           (else 
            (parse-error (format "Expected ';' or rest of statement, found ~a" out) start end))))
        ;Intermediate
        ((c-brace)
         (case kind
           ((EOF) (parse-error "Expected a } to close {" ps pe))
           ((C_BRACE) (getter))
           (else (parse-error (format "Expected a } to close open {, found ~a" out) start end))))
        ;Intermediate
        ((local)
         (unless id-ok?
           (parse-error "Found apparent variable declaration directly in an 'if', variable declarations must be in blocks"
                        ps end))
         (case kind
           ((EOF) (parse-error "Variable declaration requires a name" start end))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (output-format n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Variable declaration has not completed" start end))
                ;Just ended a local field
                ((semi-colon? n-tok) (getter))
                ((comma? n-tok) (parse-statement next (getter) 'local-list getter #t))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (if (eof? (get-tok assign-exp))
                       (parse-error (format "Expected an expression to bind to ~a" (token-value tok)) start end)
                       (parse-statement next (parse-expression null assign-exp 'start getter) 'local-init-end getter #t))))
                ((id-token? n-tok)
                 (parse-error (format "Variables must be separated by commas, ~a not allowed" n-out) start ne))
                (else (parse-error (format "Expected ';' or more variables, found ~a" n-out) start ne)))))
           (else 
            (parse-error 
             (if (keyword? tok)
                 (format "Expected a name for this variable, cannot be named reserved word ~a" kind)
                 (format "Expected a name for this variable, found ~a" out)) start end))))
        ;Intermediate
        ((local-list)
         (case kind
           ((EOF) (parse-error "Expected an additional variable after comma" ps pe))
           ((IDENTIFIER)
            (let* ((next (getter))
                   (n-tok (get-tok next))
                   (n-out (output-format n-tok))
                   (ne (get-end next)))
              (cond
                ((eof? n-tok) (parse-error "Variable is not complete" start end))
                ((semi-colon? n-tok) (getter))
                ((comma? n-tok) (parse-statement next (getter) 'local-list getter id-ok?))
                ((teaching-assignment-operator? n-tok)
                 (let ((assign-exp (getter)))
                   (if (eof? (get-tok assign-exp))
                       (parse-error (format "Expected an expression to bind to ~a" (token-value tok)) start end)
                       (parse-statement next (parse-expression null assign-exp 'start getter) 'local-init-end getter id-ok?))))
                ((id-token? n-tok)
                 (parse-error (format "Variables must be separated by commas, ~a not allowed" n-out) start ne))
                (else (parse-error (format "Expected ';' or more variables, found ~a" n-out) start ne)))))
           (else
            (parse-error
             (if (keyword? tok)
                 (format "Expected a name for this variable, cannot be named reseved word ~a" kind)
                 (format "Expected a name for this variable, found ~a" out)) start end))))
        ;Intermediate
        ((local-init-end)
         (case kind
           ((EOF) (parse-error "Expected a ; or comma after variable" ps pe))
           ((COMMA) (parse-definition cur-tok (getter) 'local-list getter))
           ((SEMI_COLON) (getter))
           ((IDENTIFIER) (parse-error (format "Variables must be separated by commas, ~a not allowed" out) start end))
           (else (parse-error (format "Expected a ; to end variable, or more variables, found ~a" out) start end))))                
        )))
  
  ;parse-expression: token token state (->token) -> token
  (define (parse-expression pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (kind (get-token-name tok))
           (out (output-format tok))
           (start (get-start cur-tok))
           (end (get-end cur-tok))
           (ps (if (null? pre) null (get-start pre)))
           (pe (if (null? pre) null (get-end pre))))
               
      (case state
        ((start)
         (case kind
           ((EOF) (parse-error "Expected an expression" ps pe))
           ((~ ! -) (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter) 'op-or-end getter))
           ((+) 
            (if (eq? (level) 'intermediate)
                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter) 'op-or-end getter)
                (parse-error "Expected an expression, + cannot begin an expression" start end)))
           ((NULL_LIT) 
            (if (eq? (level) 'intermediate)
                (parse-expression cur-tok (getter) 'dot-op-or-end getter)
                (parse-error "Expected an expression. null may not be used here" start end)))
           ((TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this)
            (parse-expression cur-tok (getter) 'dot-op-or-end getter))
           ((O_PAREN)
            (if (eq? (level) 'intermediate)
                (parse-expression cur-tok (getter) 'cast-or-parened getter)
                (parse-expression cur-tok (parse-expression cur-tok (getter) 'start getter) 'c-paren getter)))
           ((new) (parse-expression cur-tok (getter) 'class-alloc-start getter))
           ((IDENTIFIER) (parse-expression cur-tok (getter) 'name getter))
           ((STRING_ERROR)
            (if (eq? 'STRING_NEWLINE (get-token-name (caddr (token-value tok))))
                (parse-error (format "A string must be contained all on one line, and end in '~a'" #\") start end)
                (parse-error (format "String must end with '~a', which is not found" #\") start end)))
           (else 
            (parse-error (format "Expected an expression, ~a is not the valid beginning of an expression" out) start end))))
        ((op-or-end)
         (if (bin-operator? tok)
             (parse-expression cur-tok (getter) 'start getter)
             cur-tok))
        ((dot-op-or-end)
           (cond
             ((dot? tok) 
              (let* ((next (getter))
                    (next-tok (get-tok next))
                    (name (get-token-name next-tok))
                    (ns (get-start next))
                    (ne (get-end next)))
                (cond 
                  ((id-token? next-tok)
                   (let* ((afterID (getter)))
                     (if (o-paren? (get-tok afterID))
                         (parse-expression next afterID 'method-call-args getter)
                         (parse-expression next afterID 'dot-op-or-end getter))))
                  ((keyword? next-tok) 
                   (parse-error (format "Expected a method name, reserved name ~a may not be a method name" name) ns ne))
                  (else (parse-error (format "Expected a method name, found ~a" (output-format next-tok)) ns ne)))))
             ((bin-operator? tok) (parse-expression cur-tok (getter) 'start getter))
             (else cur-tok)))
        ;Intermediate
        ((cast-or-parened)
         (cond
           ((eof? tok) (parse-error "Expected a name or expression and a )" ps pe))
           ((prim-type? tok) (parse-expression pre (getter) 'cast getter))
           ((id-token? tok) (parse-expression pre (getter) 'cast-or-parened-close getter))
           (else (parse-expression pre (parse-expression pre cur-tok 'start getter) 'c-paren getter))))
        ;Intermediate
        ((cast)
         (cond
           ((eof? tok) (parse-error "cast must have close paren and additional expression" ps pe))
           ((c-paren? tok) (parse-expression cur-tok (getter) 'start getter))           
           (parse-error (format "cast must have close paren, found ~a instead" out) ps end)))
        ;Intermediate
        ((cast-or-parened-close)
         (cond
           ((eof? tok) (parse-error "Expected a ')'" ps pe))
           ((c-paren? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))              
              (case (get-token-name next-tok)
                ((~ ! - + TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this O_PAREN new IDENTIFIER)
                 (parse-expression cur-tok next 'start getter))
                ((NULL_LIT)
                 (if (eq? (level) 'intermediate) 
                     (parse-expression cur-tok next 'start getter)
                     (parse-expression cur-tok next 'dot-op-or-end getter)))
                (else (parse-expression cur-tok next 'dot-op-or-end getter)))))
           (else (parse-error (format "Expected a ')', found ~a" out) ps end))))              
        ((c-paren)
         (cond
           ((eof? tok) (parse-error "Expected a )" ps pe))
           ((c-paren? tok) (parse-expression cur-tok (getter) 'dot-op-or-end getter))
           (else (parse-error (format "Expression in parens must have close paren, found ~a instead" out) ps end))))
        ((class-alloc-start)
         (cond
           ((id-token? tok)
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected constructor arguments for class allocation" start end))
                ((dot? next-tok) 
                 (parse-expression next (parse-name (getter) getter) 'class-args-start getter))
                ((o-paren? next-tok) (parse-expression cur-tok next 'class-args-start getter))
                ((open-separator? next-tok) 
                 (parse-error (format "Expected ( to begin constructor arguments, found ~a" (get-token-name next-tok))
                              (get-start next) (get-end next)))
                (else (parse-error (format "Expected constructor arguments in parens, found ~a" (output-format next-tok))
                                   (get-start next) (get-end next))))))
           ((keyword? tok) (parse-error (format "Expected a class name, reserved word ~a is not a class" kind) start end))
           (else (parse-error (format "Expected a class name, found ~a" out) start end))))
        ((class-args-start)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments starting with (" ps pe))
           ((O_PAREN) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error "Expected constructor arguments or )" start end))
                ((c-paren? next-tok) (parse-expression next (getter) 'dot-op-or-end getter))
                (else (parse-expression next (parse-expression cur-tok next 'start getter) 'class-args getter)))))
           (else (parse-error (format "Expected constructor arguments, starting with (, found ~a" out) start end))))
        ((class-args)
         (case kind
           ((EOF) (parse-error "Expected constructor arguments or )" ps pe))
           ((C_PAREN) (parse-expression cur-tok (getter) 'dot-op-or-end getter))
           ((COMMA) 
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-expression cur-tok (parse-expression cur-tok next 'start getter) 'class-args getter))))
           (else 
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close constructor arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between constructor arguments, found ~a" out) start end)))))
        ((name)
         (case kind
           ((PERIOD) (parse-expression cur-tok (parse-name (getter) getter) 'name getter))
           ((O_PAREN) (parse-expression pre cur-tok 'method-call-args getter))
           (else (parse-expression pre cur-tok 'op-or-end getter))))
        ((method-call-args)
         (case kind
           ((EOF) (parse-error "Expected method arguments starting with (" ps pe))
           ((O_PAREN) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (parse-error "Expected method arguments or )" start end))
                ((c-paren? (car next-tok)) (parse-expression next-tok (getter) 'dot-op-or-end getter))
                (else (parse-expression cur-tok (parse-expression cur-tok next-tok 'start getter) 'method-args getter)))))
           (else (parse-error (format "Expected method arguments in parens, found ~a" out) start end))))
        ((method-args)
         (case kind
           ((EOF) (parse-error "Expected method arguments or )" ps pe))
           ((C_PAREN) (parse-expression cur-tok (getter) 'dot-op-or-end getter))
           ((COMMA) 
            (let ((next (getter)))
              (if (comma? (get-tok next))
                  (parse-error "Found ',,' Only one comma is needed to separate arguments" start (get-end next))
                  (parse-expression cur-tok (parse-expression cur-tok next 'start getter) 'method-args getter))))
           (else 
            (if (close-separator? tok)
                (parse-error (format "Expected ) to close method arguments, found ~a" out) start end)
                (parse-error (format "A ',' is required between method arguments, found ~a" out) start end))))))))
      
  )
  
 