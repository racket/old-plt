(unit/sig xml-lex^ (import)
  
  (read-case-sensitive #t)
  ;character->int conversion for often queried characters    
  (define lt (char->integer #\<))
  (define gt (char->integer #\>))
  (define bang (char->integer #\!))
  (define slash (char->integer #\/))
  (define nl (char->integer #\newline))
  (define sp (char->integer #\space))
  (define tab (char->integer #\tab))
  (define ret (char->integer #\return))
  (define colon (char->integer #\:))
  (define score (char->integer #\_))
  (define dot (char->integer #\.))
  (define minus (char->integer #\-))
  (define lp (char->integer #\())
  (define rp (char->integer #\)))
  (define plus (char->integer #\+))
  (define star (char->integer #\*))
  (define comma (char->integer #\,))
  (define line (char->integer #\|))
  (define qmark (char->integer #\?))
  (define amp (char->integer #\&))
  (define hash (char->integer #\#))
  (define quot (char->integer #\"))
  (define apos (char->integer #\'))
  (define percent (char->integer #\%))
  (define rb (char->integer #\[))
  (define lb (char->integer #\]))
  (define eq (char->integer #\=))
  (define scolon (char->integer #\;))
  
  ;Lexer from port
  ;Lexer reads in characters, converts to ascii numbers
  
  ;Structure to create a stream of ints from a file
  (define-struct fstream (char rest))
  
  ;open-next: fstream -> fstream
  ;Applies the function of the rest of the fstream and replaces it as the rest
  (define open-next
    (lambda (c)
      (set-fstream-rest! c ((fstream-rest c)))
      c))
  
  ;build-fstream: string -> make-fstream
  ;opens file identified by string, read's chars into fstream
  (define (build-fstream file-name)
    (call-with-input-file file-name port->fstream))
  
  ;; port->fstream : Input-port -> Fstream
  (define (port->fstream p)
    (local 
        ((define f 
           (lambda ()
             (if (eof-object? (peek-char p))
                 null
                 (make-fstream (list (char->integer (read-char p))
                                     (file-position p)) f)))))
      (f)))
  
  
  ;Helper functions used in tokenizing xml documents
  
  ;whitspace?: int -> bool
  (define whitespace? 
    (lambda (ch)
      (or (eq? ch nl)
          (eq? ch sp)
          (eq? ch tab)
          (eq? ch ret))))
  
  ;name?: int -> bool
  ;Determines whether given int represents the first character of a name
  (define name?
    (lambda (ch)
      (or (char-alphabetic? (integer->char ch))
          (eq? colon ch)
          (eq? score ch))))
  
  ;NameChar?: int -> bool
  ;Determines whether int is a legal character in a name
  (define NameChar?
    (lambda (ch)
      (or (name? ch)
          (char-numeric? (integer->char ch))
          (eq? ch dot)
          (eq? ch minus))))
  
  ;build-name: fstream bool string -> list string fstream
  (define build-name
    (lambda (istream start name)
      (if (null? istream) 
          (raise (make-lex:error 
                  (string-append "File should not end in the middle of " name) name))
          (if start
              (if (name? (car (fstream-char istream)))
                  (build-name ((fstream-rest istream)) #f
                              (string-append name
                                             (string (integer->char (car (fstream-char istream))))))
                  (if (whitespace? (car (fstream-char istream)))
                      (build-name ((fstream-rest istream)) #t name)
                      (raise (make-lex:error 
                              (string-append "Names cannot begin with " 
                                             (string 
                                              (integer->char (car (fstream-char istream)))))
                              istream))))
              (if (NameChar? (car (fstream-char istream)))
                  (build-name ((fstream-rest istream)) start
                              (string-append name
                                             (string (integer->char (car (fstream-char istream))))))
                  (list name istream))))))
  
  
  ;find-kind: fstream ints string-> list string fstream
  ;Builds the word of the kind of dToken to create, passes to function which creates
  (define find-kind
    (lambda (istream word)
      (if (null? istream) 
          (raise (make-lex:error 
                  (string-append "File should not end in the middle of " word)
                  word))
          (if (char-alphabetic? (integer->char (car (fstream-char istream))))
              (find-kind ((fstream-rest istream)) 
                         (string-append word (string (integer->char (car (fstream-char istream))))))
              (if (whitespace? (car (fstream-char istream)))
                  (list word ((fstream-rest istream)))
                  (raise (make-lex:error 
                          (string-append word " is not a legal declaration keyword") word)))))))
  
  ;; build-attVal: fstream null Nat -> list (list char or entRef) fstream
  (define build-attVal
    (lambda (istream val end)
      (if (null? istream) 
          (raise (make-lex:error "File should not end in the middle of attvalue" val))
          (cond
            ((= (car (fstream-char istream)) end)
             (list (reverse val) ((fstream-rest istream))))
            ((eq? (car (fstream-char istream)) amp)
             (let ((ent-stream (build-entref-intern ((fstream-rest istream)))))
               (build-attVal (cadr ent-stream) (cons (car ent-stream) val) end)))
            ((eq? (car (fstream-char istream)) lt) (raise (make-lex:error "Attvals cannot have <" val)))
            (else 
             (build-attVal ((fstream-rest istream)) (cons (integer->char (car (fstream-char istream)))
                                                          val) end))))))
  
  ;strip-off-whitespace: fstream->fstream
  (define strip-off-whitespace
    (lambda (istream)
      (if (null? istream) 
          (raise (make-lex:error "File should not end before end-tag" istream))
          (if (whitespace? (car (fstream-char istream)))
              (strip-off-whitespace ((fstream-rest istream)))
              istream))))
  
  ;stream of token
  (define-struct tokstream (token rest))
  
  ;;token where a token is    
  ;(make-begin-tag symbol (list string) num num)
  ;(make-pi-tag symbol (list string) num num)
  ;(make-doc-tag symbol (list string) num num)
  ;(make-cdata-tag (list characters) num num)
  ;(make-empty-tag symbol (list string) num num)
  ;(make-end-tag symbol num num)
  ;(make-text (list char | entity-ref) num num)
  ;(make-entity-ref string num num)
  
  ;tokens for an xml document
  (define-struct token ())
  (define-struct (begin-tag struct:token) (name contents start stop))
  (define-struct (pi-tag struct:token) (type contents start stop))
  (define-struct (doc-tag struct:token) (root contents start stop))
  (define-struct (cdata-tag struct:token) (contents start stop))
  (define-struct (empty-tag struct:token) (name contents start stop))
  (define-struct (end-tag struct:token) (contents start stop))
  (define-struct (text struct:token) (contents start stop))
  (define-struct (entity-ref struct:token) (contents start stop))
  
  (define-struct (lex:error struct:exn) ())
  (define-struct (internal struct:exn) ())
  
  
  ;build-tokstream: fstream -> tokstream
  ;Converts ints from fstream into appropriate tokens in a tokstream
  (define build-tokstream
    (lambda (istream)
      (cond 
        ((null? istream) null)
        ((whitespace? (car (fstream-char istream)))
         (build-tokstream ((fstream-rest istream))))
        ((eq? (car (fstream-char istream)) lt)
         (let* ((istream ((fstream-rest istream)))
                (char (car (fstream-char istream))))
           (cond 
             ((eq? char slash)
              (build-end-tag ((fstream-rest istream))))
             ((eq? char bang)
              (let* ((istream ((fstream-rest istream)))
                     (char (car (fstream-char istream))))
                (cond ((eq? char rb) (build-cdata-tag ((fstream-rest istream))))
                      ((eq? char minus)
                       (let ((istream ((fstream-rest istream))))
                         (if (eq? (car (fstream-char istream)) minus)
                             (build-tokstream (bypass-comment ((fstream-rest istream))))
                             (raise (make-lex:error "Illegal name for start tag -*" istream)))))
                      (else (build-doc-tag istream)))))
             ((eq? char qmark)
              (build-pi-tag ((fstream-rest istream))))
             ((eq? char minus)
              (let ((istream ((fstream-rest istream))))
                (if (eq? (car (fstream-char istream)) minus)
                    (build-tokstream (bypass-comment ((fstream-rest istream))))
                    (raise (make-lex:error "Illegal name for start tag -*" istream)))))
             (else (build-BoE-tag istream)))))
        ((eq? (car (fstream-char istream)) amp)
         (build-entref ((fstream-rest istream))))
        (else 
         (build-text-tag istream)))))
  
  ;build-BoE-tag: fstream -> tokstream
  ;builds a begin or empty tag from characters and places it in a tokstream
  (define build-BoE-tag
    (lambda (istream)
      (let* ((char-start-name (cadr (fstream-char istream)))
             (name-stream (build-name istream #t (make-string 0)))
             (name (string->symbol (car name-stream)))
             (attlist-stream (build-atts (cadr name-stream) null))
             (attlist (car attlist-stream))
             (istream (cadr attlist-stream)))
        (cond
          ((eq? (car (fstream-char istream)) gt)
           (make-tokstream
            (make-begin-tag name attlist char-start-name (cadr (fstream-char istream)))
            (build-tokstream ((fstream-rest istream)))))
          ((eq? (car (fstream-char istream)) slash)
           (let ((is ((fstream-rest istream))))
             (if (eq? (car (fstream-char is)) gt)
                 (make-tokstream
                  (make-empty-tag name attlist char-start-name (cadr (fstream-char is)))
                  (build-tokstream ((fstream-rest is))))
                 (raise (make-lex:error 
                         (string-append "Illegal empty tag: slash should be followed by > in tag"
                                        name "position " (number->string (cadr (fstream-char is))))
                         name)))))
          (else (raise (make-internal 
                        (string-append "build-atts: build-atts ended on character "
                                       (string (car (fstream-char istream)))) istream)))))))
  
  ;build-atts: fstream list -> list fstream
  (define build-atts
    (lambda (istream res)
      (cond
        ((null? istream) 
         (raise (make-lex:error "File should not end in the middle of attribute list" res)))
        ((or (eq? (car (fstream-char istream)) gt)
             (eq? (car (fstream-char istream)) slash))
         (list res istream))
        ((whitespace? (car (fstream-char istream)))
         (build-atts ((fstream-rest istream)) res))
        (else
         (let* ((char-name-start (cadr (fstream-char istream)))
                (name-stream (build-name istream #t (make-string 0)))
                (istream (strip-off-whitespace (cadr name-stream))))
           (if (eq? (car (fstream-char istream)) eq)
               (let* ((istream (strip-off-whitespace ((fstream-rest istream))))
		      [next-char (car (fstream-char istream))])
                 (if (or (= next-char quot) (= next-char apos))
                     (let ((att-stream (build-attVal ((fstream-rest istream)) null next-char)))
                       (build-atts (cadr att-stream) 
                                   (cons (list (car name-stream)
                                               (car att-stream)
                                               char-name-start
                                               (cadr (fstream-char (cadr att-stream)))) res)))
                     (kablooie-attribute "Attribute value for " " should be surrounded in quotes"
                                         name-stream istream)))
               (kablooie-attribute "Attribute declarations should have name=val in " ""
                                   name-stream istream)))))))
  
  (define (kablooie-attribute mess0 mess1 name-stream istream)
    (raise (make-lex:error (string-append mess0 (car name-stream)
                                          " position " (number->string (cadr (fstream-char istream)))
                                          mess1)
                           istream)))
  
  ;build-end-tag: fstream -> tokstream
  (define build-end-tag
    (lambda (istream)
      (let* ((char-start (cadr (fstream-char istream)))
             (name-stream (build-name istream #t (make-string 0)))
             (istream (strip-off-whitespace (cadr name-stream))))
        (if (eq? gt (car (fstream-char istream)))
            (make-tokstream (make-end-tag (string->symbol (car name-stream))
                                          char-start
                                          (cadr (fstream-char istream)))
                            (build-tokstream ((fstream-rest istream))))
            (raise (make-lex:error (format "End tag ~a position ~a should end with >"
                                           (car name-stream) (cadr (fstream-char istream)))
                                   istream))))))
  
  ;find-text: fstream list -> (list list fstream)
  ;; kathyg's type is wrong - ptg
  (define find-text
    (lambda (istream res stop ref)
      (cond
        ((or (null? istream)
             (eq? (car (fstream-char istream)) stop))
         (list (reverse res) istream))
        ((eq? (car (fstream-char istream)) ref)
         (let* ((ent-start (cadr (fstream-char istream)))
                (ent-stream (find-text ((fstream-rest istream)) null scolon #f))
                (istream (cadr ent-stream)))
           (find-text ((fstream-rest istream)) (cons (make-entity-ref (car ent-stream) ent-start 
                                                                      (cadr (fstream-char istream)))
                                                     res) stop ref)))
        (else
         (find-text ((fstream-rest istream)) (cons (integer->char (car (fstream-char istream)))
                                                   res) stop ref)))))
  
  ;build-text-tag: fstream -> tokstream
  (define build-text-tag
    (lambda (istream)
      (let ((text-start (cadr (fstream-char istream)))
            (text-stream (find-text istream null lt amp)))
        (make-tokstream (make-text (car text-stream) 
                                   text-start (cadr (fstream-char (cadr text-stream))))
                        (build-tokstream (cadr text-stream))))))
  
  (define DOCTYPE (string->symbol "DOCTYPE"))
  (define CDATA (string->symbol "CDATA"))
  
  ;build-doc-tag: fstream -> tokstream
  ;right now, still doesn't allow internal... about to
  (define build-doc-tag
    (lambda (istream)
      (let ((doc-start (cadr (fstream-char istream)))
            (kind-stream (find-kind istream (make-string 0))))
        (unless (eq? (string->symbol (car kind-stream)) DOCTYPE)
          (raise (make-lex:error (string-append "Tag " (car kind-stream) " at position "
                                                (number->string doc-start) 
                                                " written in doctype-style")
                                 istream)))
        (let* ((name-stream (build-name (cadr kind-stream) #t (make-string 0)))
               (text-stream (find-text (cadr name-stream) null gt #f)))
          (make-tokstream (make-doc-tag (car name-stream) (car text-stream)
                                        doc-start (cadr (fstream-char (cadr text-stream))))
                          (build-tokstream ((fstream-rest (cadr text-stream)))))))))
  
  ;build-pi-tag: fstream -> tokstream
  (define build-pi-tag 
    (lambda (istream)
      (let* ((pi-start (cadr (fstream-char istream)))
             (name-stream (build-name istream #t (make-string 0)))
             (name-list (string->list (car name-stream)))
             (text-stream (find-text (cadr name-stream) null qmark #f))
             (istream (if (null? (cadr text-stream)) null
                          ((fstream-rest (cadr text-stream))))))
        (if (null? istream)
            (raise (make-lex:error (string-append "Tag " (car name-stream) 
                                                  " at poistion " (number->string pi-start)
                                                  " must end in ?>")
                                   istream))
            (if (not (eq? (car (fstream-char istream)) gt))
                (raise (make-internal "build-pi-tag:find-text: Did not stop at ?>"))
                (make-tokstream (make-pi-tag (string->symbol (car name-stream))
                                             (car text-stream)
                                             pi-start (cadr (fstream-char istream)))
                                (build-tokstream ((fstream-rest istream)))))))))
  
  ;build-cdata-tag: fstream -> tokstream
  (define build-cdata-tag
    (lambda (istream)
      (let* ((cd-start (cadr (fstream-char istream)))
             (kind-stream (find-kind istream (make-string 0)))
             (istream (cadr kind-stream)))
        (if (not (eq? (string->symbol (car kind-stream)) CDATA))
            (raise (make-lex:error (string-append "Tag " (car kind-stream) " at position "
                                                  (number->string cd-start) " expected to be CDATA")
                                   istream))
            (if (not (eq? (car (fstream-char istream)) lb))
                (raise (make-lex:error (string-append "CDATA at position " 
                                                      (number->string (cadr (fstream-char istream))) 
                                                      " should be followed by [") istream))
                (let* ((text-stream (find-text ((fstream-rest istream)) null rb #f))
                       (istream ((fstream-rest (cadr text-stream))))
                       (ch (car (fstream-char istream)))
                       (istream ((fstream-rest istream))))
                  (if (and (eq? ch rb)
                           (eq? (car (fstream-char istream)) gt))
                      (make-tokstream (make-cdata-tag (car text-stream) 
                                                      cd-start (cadr (fstream-char istream)))
                                      (build-tokstream ((fstream-rest istream))))
                      (raise (make-lex:error (string-append "CDATA at position "
                                                            (number->string cd-start)
                                                            " does not end in ]>") istream)))))))))
  
  
  (define make-build-ent
    (lambda (maker with-rest)
      (lambda (istream)
        (let ((ent-start (cadr (fstream-char istream)))
              (text-stream (find-text istream null scolon #f)))
          (maker (make-entity-ref (car text-stream) ent-start
                                  (cadr (fstream-char (cadr text-stream))))
                 (with-rest ((fstream-rest (cadr text-stream)))))))))
  
  ;build-entref-intern: fstream -> (list entity-ref fstream)
  (define build-entref-intern
    (make-build-ent (lambda (ent rest) (list ent rest))
                    (lambda (rest) rest)))
  
  ;build-entref: fstream -> tokstream
  (define build-entref
    (make-build-ent
     make-tokstream
     (lambda (a) (build-tokstream a))))
  
  ;bypass-comment: fstream -> fstream
  (define bypass-comment
    (lambda (istream)
      (if (eq? (car (fstream-char istream)) minus)
          (let ((istream ((fstream-rest istream))))
            (if (eq? (car (fstream-char istream)) minus)
                (let ((istream ((fstream-rest istream))))
                  (if (eq? (car (fstream-char istream)) gt)
                      ((fstream-rest istream))
                      (raise (make-lex:error 
                              (string-append 
                               "Two -- in comment signal end of comment, misplaced at position "
                               (cadr (fstream-char istream)))
                              istream))))
                (bypass-comment ((fstream-rest istream)))))
          (bypass-comment ((fstream-rest istream)))))))



;(define a (build-fstream "../xml/test2.xml"))
;(define b (build-tokstream a))

;(define c (call-with-input-file "../xml/test_old/test.xml" port->fstream))
;(define d (build-tokstream c))
;(define d(call-with-input-file "../xml/prog.xml" (lambda (a) (build-tokstream (port->fstream a)))))

;(define d(call-with-input-file "../xml/test_old/test.xml" (lambda (a) (build-tokstream (port->fstream a)))))

