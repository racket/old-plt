#cs
(module beginner-error mzscheme
  
  (require "lexer.ss" "general-parsing.ss"
           "../parameters.ss"
           (lib "readerr.ss" "syntax")
           (lib "lex.ss" "parser-tools"))
  
  (provide find-beginner-error find-beginner-error-interactions)
  
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
        (case (get-token-name (car first-tok))
          ((EOF) #t)
          ((if return) (parse-statement first-tok 'start getter))
          (else (parse-expression first-tok 'start getter))))))
  
  ;temporary default error: used by errors that haven't had specfic error stuff done for them
  (define (raise-error msg start-token end-token)
    (if (null? end-token)
        (raise-read-error (format "Parse error of kind ~a, with respect to ~a"
                                  msg (get-token-name (car start-token)))
                          (file-path)
                          (position-line (cadr start-token))
                          (position-col (cadr start-token))
                          (+ (position-offset (cadr start-token)) (interactions-offset))
                          (- (position-offset (caddr start-token))
                             (position-offset (cadr start-token))))
        (raise-read-error (format "Parse error of kind ~a, with respect to ~a and ~a"
                                  msg (get-token-name (car start-token)) (get-token-name (car end-token)))
                          (file-path)
                          (position-line (cadr start-token))
                          (position-col (cadr start-token))
                          (+ (position-offset (cadr start-token)) (interactions-offset))
                          (- (position-offset (caddr end-token))
                             (position-offset (cadr start-token))))))

  ;parse-error: string position position
  (define (parse-error message start stop)
    (raise-read-error message
                      (file-path)
                      (position-line start)
                      (position-col start)
                      (+ (position-offset start) (interactions-offset))
                      (- (position-offset start)
                         (position-offset stop))))

  ;token = (list lex-token position position)
  (define (get-tok token) (car token))
  (define (get-start token) (cadr token))
  (define (get-end token) (caddr token))
  
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
      ((keyword? tok) (format "keyword ~a" (get-token-name tok)))
      ((id-token? tok) (format "identifier ~a" (token-value tok)))
      ((literal-token? tok) (format "value ~a" (token-value tok)))))
  
  ;parse-definition: token token symbol (-> token) -> void
  (define (parse-definition pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (tok-kind (get-token-name tok))
           (start (get-start cur-tok))
           (stop (get-end cur-tok)))
      
      (case state
        ((start) 
         (case tok-kind
           ((EOF) #t)
           ((class) (parse-definition cur-tok (getter) 'class-id getter))
           ((abstract) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))                   
              (cond
                ((class? next-tok) (parse-definition cur-tok next state getter))
                ((eof? next-tok) (parse-error "abstract should be followed by class definition" start stop))
                (else 
                 (if (close-to-keyword? next-tok 'class)
                     (parse-error (format "expected 'class' after 'abstract,' found ~a which is incorrectly spelled or capitalized"
                                          (token-value next-tok))
                                  start
                                  (get-end next))
                     (parse-error (format "abstract muct be immediately followed by 'class' not ~a" (output-format next-tok))
                                  start
                                  (get-end next)))))))
           (else 
            (cond
              ((close-to-keyword? tok 'class)
                (parse-error (format "expected 'class', found ~a which is incorrectly spelled or capitalized"
                                     (token-value tok))
                             start stop))
              ((close-to-keyword? tok 'abstract)
               (parse-error (format "Expected 'abstract class' or 'class', found ~a which is incorrectly spelled or capitalized"
                                    (token-value tok))
                            start stop))
              ((or (if-token? tok) (return-token? tok))
               (parse-error (format "Expected class definition, found ~a. Statements must be in a method or interactions window"
                                    (output-format tok))
                            start stop))
              ((prim-type? tok) 
               (parse-error (format "Expected class definition, found ~a. Fields and methods must be in a class body"
                                    (output-format tok)) start stop))
              ((id-token? tok)
               (parse-error (format "Expected class definition, found ~a. Fields, methods, and expressions may not be written here"
                                    (output-format tok)) start stop))
              (else
               (parse-error (format "Expected class definition, found ~a which may not be written here" (output-format tok))
                            start stop))))))
        ((class-id)
         (case tok-kind
           ((EOF) (parse-error "'class' should be followed by a class name and body" (get-start pre) (get-end pre)))
           ((IDENTIFIER) 
            (let* ((next (getter))
                   (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) (parse-error (format "expected class body after ~a" (token-value tok)) start stop))
                ((extends? next-tok) (parse-definition cur-tok next 'extends getter))
                ((o-brace? next-tok) (parse-definition cur-tok next 'body getter))
                (else 
                 (cond
                   ((close-to-keyword? next-tok 'extends) 
                    (parse-error (format "found ~a, which is similar to 'extends'" (token-value next-tok))
                                 (get-start next) (get-end next)))
                   ((open-separator? next-tok)
                    (parse-error (format "expected { to begin class body, but found ~a" (output-format next-tok))
                                 (get-start next) (get-end next)))
                   ((close-separator? tok)
                    (parse-error (format "Class body must be opened with { before being closed, found ~a" 
                                         (output-format tok)) (get-start next) (get-end next)))
                   (else
                    (parse-error (format "class name must be followed by 'extends' clause or { to start class body, found ~a"
                                         (output-format next-tok)) start (get-end next))))))))
           (else 
            (if (keyword? tok) 
                (parse-error (format "class may not be called ~a as this is a reserved term" tok-kind) start stop)
                (parse-error (format "expected a name for this class, given ~a" (output-format tok))
                             start stop)))))
        ((extends) 
         (let* ((next (getter))
                (next-tok (get-tok next)))
           (cond
             ((eof? next-tok) (parse-error "Expected parent class after extends" (get-start pre) (get-end pre)))
             ((id-token? next-tok) (parse-definition next (getter) 'body getter))
             ((o-brace? next-tok) (parse-error "Expected a parent name after extends and before the class body starts"
                                               start (get-end next)))
             ((keyword? next-tok)
              (parse-error (format "parent may not be named after reserved word ~a" (get-token-name next-tok))
                           (get-start next) (get-end next)))
             (else (parse-error (format "extends must be followed by parent name, found ~a" (output-format next-tok))
                                start (get-end next))))))
        ((body)
         (case tok-kind
           ((EOF) (parse-error (format "Expected class body to begin after ~a" (output-format (get-tok pre)))
                               (get-start pre) (get-end pre)))
           ((O_BRACE) (parse-definition cur-tok (parse-members null (getter) 'start getter) 'body-end getter))
           (else 
            (cond
              ((open-separator? tok)
               (parse-error (format "expected { to begin class body, but found ~a" (output-format tok))
                            start stop))
              ((close-separator? tok)
               (parse-error (format "Class body must be opened with { before being closed, found ~a" 
                                    (output-format tok)) start stop))
              (else
               (parse-error (format "Expected { to start class body, found ~a"
                                    (output-format tok)) start stop))))))
        ((body-end)
         (case tok-kind
           ((EOF) (parse-error "Expected a } to close this body" (get-start pre) (get-end pre)))
           ((C_BRACE) 
            (let ((next (getter)))
              (if (c-brace? (get-tok next))
                  (parse-error "Unnecessary }, class body already closed" start (get-end next))
                  (parse-definition cur-tok next 'start getter))))
           (else (parse-error (format "Expected a } to close class body, found ~a" (output-format tok))
                              (get-start pre) stop)))))))
              
  ;parse-members: token token symbol (->token) -> token
  (define (parse-members pre cur state getter) 
    (let* ((tok (get-tok cur))
           (kind (get-token-name tok))
           (out (output-format tok))
           (srt (get-start cur))
           (end (get-end cur))
           (ps (get-start pre))
           (pe (get-end pre)))

      (case state
        ((start)
         (cond
           ((or (eof? tok) (c-brace? tok)) cur)
           ((abstract? tok) (parse-members cur (getter) 'method getter))
           ((prim-type? tok) (parse-members cur (getter) 'method-or-field getter))
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
                 (parse-error (format "Expected an end to field ~a, field end in ';', ',' is not allowed" (token-value tok))
                              srt ne))
                ((o-paren? n-tok) (parse-members next (getter) 'method-parms getter))
                ((open-separator? n-tok) 
                 (parse-error (format "Method parameters must begin with ( found ~a" n-out) srt ne))
                ((id-token? n-tok)
                 (parse-error 
                  (format "Fields must be separatley declared, method paramters must be in ()s, ~a not allowed" n-out)
                  srt ne))
                (else (parse-error 
                       (format "Expected ; to end field or method parameter list, found ~a" n-out) srt ne)))))
           (else 
            (parse-error 
             (if (keyword? tok)
                 (format "Expected a name for this field or method, cannot be named reserved word ~a" kind)
                 (format "Expected a name for this field or method, found ~a" out))
             srt end))))
        ((method)
         (cond
           ((eof? tok) (parse-error "Expected method, and class body still requires a }" ps pe))
           ((prim-type? tok) (parse-members cur (getter) 'method-id getter))
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
                 (parse-members next (parse-ctor-body (getter) 'start getter) 'ctor-end getter))
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
                 (parse-members next (parse-statement (getter) 'start getter) 'method-end getter))
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
  
  ;parse-body: token token symbol (->token) -> token
  (define (parse-ctor-body pre cur-tok state getter)
    (let* ((tok (get-tok cur-tok))
           (tok-kind (get-token-name tok))
           (tok-out (output-format tok))
           (tok-start (get-start cur-tok))
           (tok-stop (get-end cur-tok))
           (p-start (get-start pre))
           (p-end (get-end pre)))
      (case state
        ((start)
         (case tok-kind
           ((C_BRACE EOF) cur-tok)
           ((this) 
            (let* ((next (getter))
                  (next-tok (get-tok next)))
              (cond
                ((eof? next-tok) 
                 (parse-error "Expected rest of field initialization, constructor and class need }" p-start p-end))
                ((dot? next-tok)
                 (let ((after-dot (getter)))
                   (cond
                     ((id-token? (car after-dot)) (parse-ctor-body (getter) 'assign-op getter))
                     ((keyword? (car after-dot)) (parse-error (format "Expected identifier after ., found reserved word ~a"
                                                                      (get-token-name (car after-dot)) (get-start next-tok)
                                                                      (get-end (car after-dot)))))
                     (else
                      (parse-error (format "Expected identifer after ., found ~a" (output-format (car after-dot))
                                           (get-start next-tok) (get-end (car after-dot))))))))
                (else (parse-error (format "Expected this.Field, found ~a instead of ." (output-format tok)) tok-start tok-stop)))))
           ((IDENTIFIER)
            (let ((next-token (getter)))
              (if (dot? (car next-token)) 
                  (parse-ctor-body (parse-name (getter) getter) 'assign-op getter)
                  (parse-ctor-body next-token 'assign-op getter))))
           (else 
            (if (keyword? tok)
                (parse-error (format "Expected name, found reserved word ~a" tok-kind) tok-start tok-stop)
                (parse-error (format "Expected name, found ~a" (output-format tok)) tok-start tok-stop)))))
        ((assign-op)
         (case tok-kind
           ((EOF) (parse-error "Expected rest of field initialization (=), constructor and class need }" p-start p-end))
           ((=) (parse-ctor-body (parse-expression (getter) 'start getter) 'assign-end getter))
           (else (parse-error ("Expected = to be used in initializing the field, found ~a" (output-format tok)) tok-start tok-stop))))
        ((assign-end)
         (cond
           ((eof? tok) (parse-error "Expected a ; to end field intialization, constructor and class need }" p-start p-end))
           ((semi-colon? tok) (parse-ctor-body (getter) 'start getter))
           (else (parse-error (format "Expected a ; to end field initialization, found ~a" (output-format tok)
                                      ) tok-start tok-stop
                              )))))))

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
           ((EOF) (raise-error 'unexpexted-end-of-input cur-tok null))
           ((O_PAREN) (parse-statement (parse-expression (getter) 'start getter) 'if-then getter))
           (else (raise-error 'if-must-be-followed-by-expression-in-parens cur-tok null))))
        ((if-then)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input cur-tok null))
           ((C_PAREN) (parse-statement (parse-statement (getter) 'start getter) 'if-else getter))
           (else (raise-error 'if-must-be-followed-by-expression-in-parens-parens-not-match cur-tok null))))
        ((if-else)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input cur-tok null))
           ((else) (parse-statement (parse-statement (getter) 'start getter) 'start getter))
           (else (raise-error 'if-then-statement-must-have-else cur-tok null))))
        ((return)
         (case tok-kind
           ((SEMI_COLON) (parse-statement (getter) 'start getter))
           (else (raise-error 'return-must-end-with-semi-colon cur-tok null)))))))
  
  ;parse-expression: token state (->token) -> token
  (define (parse-expression cur-tok state getter) 
    (let* ((tok (car cur-tok))
           (tok-kind (get-token-name tok))
           (tok-start (cadr cur-tok))
           (tok-stop (caddr cur-tok)))
      (case state
        ((start)
         (case tok-kind
           ((EOF) (raise-error 'unexpected-end-of-input cur-tok null))
           ((~ ! -) (parse-expression (parse-expression (getter) 'start getter) 'op-or-end getter))
           ((NULL_LIT TRUE_LIT FALSE_LIT STRING_LIT CHAR_LIT INTEGER_LIT 
                      LONG_LIT FLOAT_LIT DOUBLE_LIT this)
            (parse-expression (getter) 'dot-op-or-end getter))
           ((O_PAREN)
            (parse-expression (parse-expression (getter) 'start getter) 'c-paren getter))
           ((new) (parse-expression (getter) 'class-alloc-start getter))
           ((IDENTIFIER) (parse-expression (getter) 'name getter))
           (else 'invalid-start-of-expression cur-tok null)))
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
                    (raise-error 'expected-id-after-dot-after-primary cur-tok next-tok))))
             ((bin-operator? tok)
              (parse-expression (getter) 'start getter))
             (else cur-tok)))
        ((c-paren)
         (if (c-paren? tok)
             (parse-expression (getter) 'dot-op-or-end getter)
             (raise-error 'o-paren-expected-to-be-closed-at-end-of-expression cur-tok null)))
        ((class-alloc-start)
         (if (id-token? tok)
             (let ((next-token (getter)))
               (cond
                 ((dot? (car next-token)) 
                  (parse-expression (parse-name (getter) getter) 'class-args-start getter))
                 ((o-paren? (car next-token)) (parse-expression next-token 'class-args-start getter))
                 (else 'must-give-ctor-args-to-create-class-instance cur-tok next-token)))
             (raise-error 'class-alloc-must-have-identifier cur-tok null)))
        ((class-args-start)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input cur-tok null))
           ((O_PAREN) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input cur-tok null))
                ((c-paren? (car next-tok)) (parse-expression (getter) 'dot-op-or-end getter))
                (else (parse-expression (parse-expression next-tok 'start getter) 'class-args getter)))))
           (else (raise-error 'need-ctor-args cur-tok null))))
        ((class-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input cur-tok null))
           ((C_PAREN) (parse-expression (getter) 'dot-op-or-end getter))
           ((COMMA) (parse-expression (parse-expression (getter) 'start getter) 'class-args getter))
           (else (raise-error 'need-comma-between-ctor-args cur-tok null))))
        ((name)
         (case tok-kind
           ((PERIOD) (parse-expression (parse-name (getter) getter) 'name getter))
           ((O_PAREN) (parse-expression cur-tok 'method-call-args getter))
           (else (parse-expression cur-tok 'op-or-end getter))))
        ((method-call-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input cur-tok null))
           ((O_PAREN) 
            (let ((next-tok (getter)))
              (cond
                ((eof? (car next-tok)) (raise-error 'invalid-end-of-input cur-tok null))
                ((c-paren? (car next-tok)) (parse-expression (getter) 'dot-op-or-end getter))
                (else (parse-expression (parse-expression next-tok 'start getter) 'method-args getter)))))
           (else (raise-error 'need-call-args cur-tok null))))
        ((method-args)
         (case tok-kind
           ((EOF) (raise-error 'invalid-end-of-input cur-tok null))
           ((C_PAREN) (parse-expression (getter) 'dot-op-or-end getter))
           ((COMMA) (parse-expression (parse-expression (getter) 'start getter) 'method-args getter))
           (else (raise-error 'need-comma-between-method-args cur-tok null)))))))
      
  )
  
 