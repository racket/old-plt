(unit/sig reader^ (import xml-structs^ xml-lex^)

  (define-struct (xml-read:error struct:exn) ())
  
  ;; read-xml : [Input-port] -> Document
  (define (read-xml . input)
    (read-from-port
     (cond
       [(null? input) (current-input-port)]
       [else (car input)])))
  
  ;; read-from-port : Input-port -> Document
  (define (read-from-port input)
    (local ((define stack null)
            ;pop: -> void | #f
            (define pop 
              (lambda () 
                (if (null? stack) #f
                    (set! stack (cdr stack)))))
            ;push: symbol -> void
            (define push
              (lambda (name) (set! stack (cons name stack))))
            ;peek: -> symbol | #f
            (define peek
              (lambda () 
                (if (null? stack) #f
                    (car stack))))
            
            ;make-att-list: (list strings) -> (list attribute)
            ;; kathyg's type is wrong - ptg
            (define (make-att-list lst)
              (if (null? lst)
                  null
                  (if (pair? (car lst))
                      (if (= (length (car lst)) 4)
                          (cons (make-attribute
                                 (caddar lst) 
                                 (cadddr (car lst))
                                 (string->symbol (caar lst))
                                 (list->string (expand-ref (cadar lst))))
                                (make-att-list (cdr lst)))
                          (raise 
                           (make-internal 
                            "read:make-att-list: Lexer didn't separate attributes" lst)))
                      (raise (make-internal
                              "read:make-att-lsit: attribute not nested in a list" lst)))))
            
            
            ;chars->strings: (listof char) char -> (listof string)
            (define chars->strings
              (lambda (lst stop)
                (if (null? lst) null
                    (let ((stls (build-string lst (make-string 0) stop)))
                      (if (equal? (car stls) (make-string 0))
                          (chars->strings (cadr stls) stop)
                          (cons (car stls) (chars->strings (cadr stls) stop)))))))
            
            ;build-string: (list of char) string char -> (list string (list of char))
            (define build-string
              (lambda (lst str stop)
                (cond
                  ((null? lst) (list str lst))
                  ((char-whitespace? (car lst))
                   (list str (cdr lst)))
                  ((eq? (car lst) stop)
                   (list (string #\=) (cdr lst)))
                  ((and (not (null? (cdr lst)))
                        (eq? (cadr lst) stop))
                   (list (string-append str (string (car lst)))
                         (cdr lst)))
                  (else 
                   (build-string 
                    (cdr lst)
                    (string-append str (string (car lst))) stop)))))
            
            
            ;; read-document : tokenstream document-> document
            ;; This does not follow the data def - ptg
            (define (read-document toks document)
              (let ((token (if (not (null? toks))
                               (tokstream-token toks))))
                (cond
                  ((null? toks) document)
                  ((and (null? (document-prolog document))
                        (or (pi-tag? token)
                            (doc-tag? token)))
                   (let ((prolog-toks (build-prolog toks (make-prolog null null null null))))
                     (read-document (cadr prolog-toks) 
                                    (make-document (car prolog-toks) null null))))
                  ((pi-tag? token)
                   (if (eq? (pi-tag-type token) 'xml)
                       (raise 
                        (make-xml-read:error "xml declaration allowed only at top" token))
                       (read-document (tokstream-rest toks)
                                      (make-document
                                       (document-prolog document)
                                       (document-element document)
                                       (cons (make-pi (pi-tag-type token)
                                                      (chars->strings (pi-tag-contents token) #f))
                                             (document-misc document))))))
                  ((or (begin-tag? token) (empty-tag? token)) 
                   (let ((ele-stream (build-element toks)))
                     (if (peek)
                         (raise (make-xml-read:error 
                                 (string-append (peek) " tag not closed") ele-stream))
                         (read-document (tokstream-rest (cadr ele-stream))
                                        (make-document (document-prolog document)
                                                       (car ele-stream)
                                                       null)))))
                  (else 
                   (raise (make-xml-read:error 
                           "xml document must open with prolog or element" toks))))))
            
            ;walk: (token xmlD (list pi) doctype (list pi) -> xmlD)
            ;      (token xmlD (list pi) doctype (list pi) -> pi)
            ;      (token xmlD (list pi) doctype (list pi) -> dcotype) -> 
            ;                                 (tokstream token prolog -> (list prolog tokenstream)
            (define walk
              (lambda (xml-dec-func pi-func doc-func)
                (lambda (token toks pro)
                  (let ((xml (prolog-xml pro))
                        (before-dtd (prolog-before-dtd pro))
                        (dtd (prolog-dtd pro))
                        (before-element (prolog-before-element pro)))
                    (cond
                      ((pi-tag? token)
                       (if (eq? (pi-tag-type token) 'xml)
                           (build-prolog
                            (tokstream-rest toks)
                            (xml-dec-func token xml before-dtd dtd before-element))
                           (build-prolog
                            (tokstream-rest toks)
                            (pi-func token toks xml before-dtd dtd before-element))))
                      ((doc-tag? token)
                       (build-prolog
                        (tokstream-rest toks)
                        (doc-func token xml before-dtd dtd before-element)))
                      ((or (begin-tag? token)
                           (empty-tag? token))
                       (list pro toks))
                      (else 
                       (raise 
                        (make-xml-read:error 
                         "PI, DOC, BEGIN, or EMPTY must preceed other tag types"
                         token))))))))
            
            ;build-prolog: tokenstream prolog -> (list prolog tokenstream)
            ;; This is wrong.  It doesn't follow the Data type definition for prologs in the xml-spec.
            ;;  -- ptg
            (define build-prolog
              (lambda (toks pro)
                (if (null? toks)
                    (raise (make-xml-read:error "xml document must have a body" toks))
                    (let ((token (tokstream-token toks)))
                      (cond
                        ;First time, no xml declaration set
                        ((null? (prolog-xml pro)) 
                         ((walk
                           (lambda (token xml _ __ ___)
                             (letrec ((content (chars->strings (pi-tag-contents token) #\=))
                                      (version (member "version" content))
                                      (encoding (member "encoding" content))
                                      (standalone (member "standalone" content))
                                      (test 
                                       (lambda (lst end)
                                         (if (and lst (>= (length lst) 3)
                                                  (equal? "=" (cadr lst)))
                                             (caddr lst)
                                             (end)))))
                               (make-prolog 
                                (make-xmlD 
                                 (test version 
                                       (lambda () 
                                         (raise (make-xml-read:error 
                                                 "xml declare must have version= VersionNum" 
                                                 null))))
                                 (test encoding (lambda () #f))
                                 (test standalone (lambda () #f)))
                                _ __ ___)))
                           (lambda (token _ misc __ ___)
                             (make-prolog #f
                                          (cons (make-pi (pi-tag-type token)
                                                         (chars->strings (pi-tag-contents token) #f))
                                                misc)
                                          __ ___))
                           (lambda (token _ __ ___ ____)
                             (make-prolog  #f null
                                           (make-doctype (string->symbol (doc-tag-root token))
                                                         (doc-tag-contents token)
                                                         #f)
                                           null)))
                          token toks pro))
                        ((null? (prolog-dtd pro))
                         ((walk
                           (lambda (a b c d e) 
                             (raise (make-xml-read:error "XML declaration must be at top" a)))
                           (lambda (token xml misc __ ___)
                             (make-prolog  xml
                                           (cons 
                                            (make-pi (pi-tag-type token)
                                                     (chars->strings (pi-tag-contents token)#f))
                                            misc)
                                           __ ___))
                           (lambda (token xml misc _ __)
                             (make-prolog xml misc
                                          (make-doctype (string->symbol (doc-tag-root token))
                                                        (doc-tag-contents token)
                                                        #f)
                                          __))) token toks pro))
                        (else
                         ((walk
                           (lambda (a b c d e)
                             (raise (make-xml-read:error "XML declaration must be at top" a)))
                           (lambda (token xml misc dtd misc2)
                             (make-prolog xml misc dtd
                                          (cons 
                                           (make-pi (pi-tag-type token)
                                                    (chars->strings (pi-tag-contents token)))
                                           misc2)))
                           (lambda (a b c d e) 
                             (raise (make-xml-read:error 
                                     "Detected illegal second doc declaration" a))))
                          token toks pro)))))))
            
            ;build-element: tokstream -> (list element tokstream)
            (define build-element
              (lambda (toks)
                (local ((define get-next (lambda () (set! toks (tokstream-rest toks))))
                        (define element-get
                          (lambda ()
                            (if (null? toks)
                                (raise (make-xml-read:error "File should not end before end tag"
                                                            toks))
                                (let ((token (tokstream-token toks)))
                                  (cond
                                    ((begin-tag? token)
                                     (push (begin-tag-name token))
                                     (let-values ([(contents end-pos) (build-contents)])
                                       (make-element
                                        (begin-tag-start token)
                                        end-pos
                                        (begin-tag-name token)
                                        (make-att-list (begin-tag-contents token))
                                        contents)))
                                    ((empty-tag? token)
                                     (make-element 
                                      (empty-tag-start token)
                                      (empty-tag-stop token)
                                      (empty-tag-name token)
                                      (make-att-list (empty-tag-contents token))
                                      null))
                                    (else 
                                     (raise 
                                      (make-xml-read:error 
                                       "Begin or Empty must come before end tags" token))))))))
                        
                        ;; build-contents : -> (listof Contents) x Location
                        (define (build-contents)
                          (get-next)
                          (if (null? toks) 
                              (raise (make-xml-read:error "File should not end before end tag"
                                                          toks))
                              (let ((token (tokstream-token toks)))
                                (if (end-tag? token) 
                                    (if (eq? (peek) (end-tag-contents token))
                                        (begin
                                          (pop)
                                          (values null (end-tag-stop token)))
                                        (raise (make-xml-read:error 
                                                (string-append 
                                                 "Expected end of " (symbol->string (peek)) 
                                                 " tag. Given " (symbol->string (end-tag-contents token))
                                                 " at position " (number->string (end-tag-start token)))
                                                (peek))))
                                    (let-values ([(x)(cond
                                                       ((text? token)
                                                        (make-pcdata
                                                         (text-start token)
                                                         (text-stop token)
                                                         (list->string (expand-ref (text-contents token)))))
                                                       ((cdata-tag? token) 
                                                        (make-pcdata
                                                         (cdata-tag-start token)
                                                         (cdata-tag-stop token)
                                                         (list->string (cdata-tag-contents token))))
                                                       ((entity-ref? token) 
                                                        (let ([ref (look-up-ref token)])
                                                          (cond
                                                            [(entity? ref) ref]
                                                            [else (make-pcdata 
                                                                   (entity-ref-start token)
                                                                   (entity-ref-stop token)
                                                                   ref)])))
                                                       ((pi-tag? token) 
                                                        (make-pi 
                                                         (pi-tag-start token)
                                                         (pi-tag-stop token)
                                                         (pi-tag-type token) 
                                                         (chars->strings (pi-tag-contents token) #f)))
                                                       (else (element-get)))]
                                                 [(content end-position) (build-contents)])
                                      (values (cons x content) end-position)))))))
                  (let ((element (element-get)))
                    (list element toks)))))
            
            ;expand-ref: (list char | entref) -> (list char)
            (define expand-ref
              (lambda (lst)
                (if (null? lst) 
                    null
                    (if (char? (car lst))
                        (cons (car lst) (expand-ref (cdr lst)))
                        (append (string->list 
                                 (look-up-ref (car lst)))
                                (expand-ref (cdr lst)))))))
            
            ;; digits->char : String Nat -> Char
            (define (digits->char digits base)
              (let ([num (string->number digits base)])
                (if num
                    (string (integer->char num))
                    (raise (make-xml-read:error "Invalid numeric entity" digits)))))
            
            ;; look-up-ref : Entity-ref -> String
            (define (look-up-ref ent-ref)
              (let* ([ent (list->string (entity-ref-contents ent-ref))]
                     [ent-sym (string->symbol ent)])
                (case ent-sym
                  [(lt) "<"]
                  [(gt) ">"]
                  [(amp) "&"]
                  [(apos) "'"]
                  [(quot) "\""]
                  [else
                   (let ([len (string-length ent)])
                     (cond
                       ((eq? (string-ref ent 0) #\#)
                        (cond
                          [(< len 2)
                           (raise (make-xml-read:error "Invalid numeric entity &#;" ent))]
                          [(eq? (string-ref ent 1) #\x) 
                           (digits->char (substring ent 2 len) 16)]
                          [else 
                           (digits->char (substring ent 1 len) 10)]))
                       (else ;(make-entity ent-sym) this breaks too many things -ptg yuck
                             "")))]))))
      
      (read-document (build-tokstream (port->fstream input)) (make-document null null null)))))
