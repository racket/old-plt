(module reader mzscheme
  (require (lib "unitsig.ss")
           (lib "list.ss"))
  
  (require "sig.ss")
  
  (provide reader@)
  
  (define reader@
    (unit/sig reader^
      (import xml-structs^)
      
      ;; Start-tag ::= (make-start-tag Location Location Symbol (listof Attribute))
      (define-struct (start-tag source) (name attrs))
      
      ;; End-tag ::= (make-end-tag Location Location Symbol)
      (define-struct (end-tag source) (name))
      
      ;; Token ::= Contents | Start-tag | End-tag | Eof
      
      (define read-comments (make-parameter #f))
      (define collapse-whitespace (make-parameter #f))
      
      ;; read-xml : [Input-port] -> Document
      (define read-xml
        (case-lambda
          [(in) (read-from-port in)]
          [() (read-from-port (current-input-port))]))
      
      ;; read-from-port : Input-port -> Document
      (define (read-from-port in)
        (let*-values ([(in pos) (positionify in)]
                      [(misc0 start) (read-misc in pos)])
          (make-document (make-prolog misc0 #f)
                         (cond
                           [(start-tag? start) (read-element start in pos)]
                           [(element? start) start]
                           [else (error 'read-xml "expected root element - received ~a" start)])
                         (let-values ([(misc1 end-of-file) (read-misc in pos)])
                           (unless (eof-object? end-of-file)
                             (error 'read-xml "extra stuff at end of document ~a" end-of-file))
                           misc1))))
      
      ;; read-misc : Input-port (-> Location) -> (listof Misc) Token
      (define (read-misc in pos)
        (let read-more ()
          (let ([x (lex in pos)])
            (cond
              [(or (pi? x) (comment? x))
               (let-values ([(lst next) (read-more)])
                 (values (cons x lst) next))]
              [(and (pcdata? x) (andmap char-whitespace? (string->list (pcdata-string x))))
               (read-more)]
              [else (values null x)]))))
      
      ;; read-element : Start-tag Input-port (-> Location) -> Element
      (define (read-element start in pos)
        (let ([name (start-tag-name start)]
              [a (source-start start)]
              [b (source-stop start)])
          (let read-content ([k (lambda (body end-loc)
                                  (make-element
                                   a end-loc name (start-tag-attrs start)
                                   body))])
            (let ([x (lex in pos)])
              (cond
                [(eof-object? x)
                 (error 'read-xml "unclosed `~a' tag at [~a ~a]"
                        name
                        (format-source a)
                        (format-source b))]
                [(start-tag? x) (read-content (lambda (body end-loc)
                                                (k (cons (read-element x in pos) body)
                                                   end-loc)))]
                [(end-tag? x)
                 (let ([end-loc (source-stop x)])
                   (unless (eq? name (end-tag-name x))
                     (error 'read-xml "start tag `~a' at [~a ~a] doesn't match end tag `~a' at [~a ~a]"
                            name
                            (format-source a)
                            (format-source b)
                            (end-tag-name x)
                            (format-source (source-start x))
                            (format-source end-loc)))
                   (k null end-loc))]
                [(entity? x) (read-content (lambda (body end-loc)
                                             (k (cons (expand-entity x) body)
                                                end-loc)))]
                [(comment? x) (if (read-comments)
                                  (read-content (lambda (body end-loc) (k (cons x body) end-loc)))
                                  (read-content k))]
                [else (read-content (lambda (body end-loc) (k (cons x body) end-loc)))])))))
      
      ;; expand-entity : Entity -> (U Entity Pcdata)
      ;; more here - allow expansion of user defined entities
      (define (expand-entity x)
        (let ([expanded (default-entity-table (entity-text x))])
          (if expanded
              (make-pcdata (source-start x) (source-stop x) expanded)
              x)))
      
      ;; default-entity-table : Symbol -> (U #f String)
      (define (default-entity-table name)
        (case name
          [(amp) "&"]
          [(lt) "<"]
          [(gt) ">"]
          [(quot) "\""]
          [(apos) "'"]
          [else #f]))
      
      ;; lex : Input-port (-> Location) -> Token
      (define (lex in pos)
        (let ([c (peek-char in)])
          (cond
            [(eof-object? c) c]
            [(eq? c #\&) (lex-entity in pos)]
            [(eq? c #\<) (lex-tag-cdata-pi-comment in pos)]
            [else (lex-pcdata in pos)])))
      
      ;; lex-entity : Input-port (-> Location) -> Entity
      (define (lex-entity in pos)
        (let ([start (pos)])
          (read-char in)
          (let ([data (case (peek-char in)
                        [(#\#)
                         (read-char in)
                         (let ([n (case (peek-char in)
                                    [(#\x) (read-char in)
                                     (string->number (read-until #\; in pos) 16)]
                                    [else (string->number (read-until #\; in pos))])])
                           (unless (number? n)
                             (lex-error in pos "malformed numeric entity"))
                           n)]
                        [else
                         (begin0
                           (lex-name in pos)
                           (unless (eq? (read-char in) #\;)
                             (lex-error in pos "expected ; at the end of an entity")))])])
            (make-entity start (pos) data))))
      
      ;; lex-tag-cdata-pi-comment : Input-port (-> Location) -> Start-tag | Element | End-tag | Pcdata | Pi | Comment
      (define (lex-tag-cdata-pi-comment in pos)
        (let ([start (pos)])
          (read-char in)
          (case (non-eof peek-char in pos)
            [(#\!)
             (read-char in)
             (case (non-eof peek-char in pos)
               [(#\-) (read-char in)
                (unless (eq? (read-char in) #\-)
                  (lex-error in pos "expected second - after <!-"))
                (let ([data (lex-comment-contents in pos)])
                  (unless (eq? (read-char in) #\>)
                    (lex-error in pos "expected > to end comment (\"--\" can't appear in comments)"))
                  ;(make-comment start (pos) data)
                  (make-comment data))]
               [(#\[) (read-char in)
                (unless (string=? (read-string 6 in) "CDATA[")
                  (lex-error in pos "expected CDATA following <["))
                (let ([data (lex-cdata-contents in pos)])
                  (make-pcdata start (pos) data))]
               [else (skip-dtd in pos)
                     (skip-space in)
                     (unless (eq? (peek-char in) #\<)
                       (lex-error in pos "expected pi, comment, or element after doctype"))
                     (lex-tag-cdata-pi-comment in pos)])]
            [(#\?) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (let ([data (lex-pi-data in pos)])
                 (make-pi start (pos) name data)))]
            [(#\/) (read-char in)
             (let ([name (lex-name in pos)])
               (skip-space in)
               (unless (eq? (read-char in) #\>)
                 (lex-error in pos "expected > to close ~a's end tag" name))
               (make-end-tag start (pos) name))]
            [else
             (let ([name (lex-name in pos)]
                   [attrs (lex-attributes in pos)])
               (skip-space in)
               (case (read-char in)
                 [(#\/)
                  (unless (eq? (read-char in) #\>)
                    (lex-error in pos "expected > to close empty element ~a" name))
                  (make-element start (pos) name attrs null)]
                 [(#\>) (make-start-tag start (pos) name attrs)]
                 [else (lex-error in pos "expected / or > to close tag `~a'" name)]))])))
      
      ;; lex-attributes : Input-port (-> Location) -> (listof Attribute)
      (define (lex-attributes in pos)
        (quicksort (let loop ()
                     (skip-space in)
                     (cond
                       [(name-start? (peek-char in))
                        (cons (lex-attribute in pos) (loop))]
                       [else null]))
                   (lambda (a b)
                     (let ([na (attribute-name a)]
                           [nb (attribute-name b)])
                       (cond
                         [(eq? na nb) (lex-error in pos "duplicated attribute name ~a" na)]
                         [else (string<? (symbol->string na) (symbol->string nb))])))))
      
      ;; lex-attribute : Input-port (-> Location) -> Attribute
      (define (lex-attribute in pos)
        (let ([start (pos)]
              [name (lex-name in pos)])
          (skip-space in)
          (unless (eq? (read-char in) #\=)
            (lex-error in pos "expected = in attribute ~a" name))
          (skip-space in)
          ;; more here - handle entites and disallow "<"
          (let* ([delimiter (read-char in)]
                 [value (case delimiter
                          [(#\' #\")
                           (list->string
                            (let read-more ()
                              (let ([c (non-eof peek-char in pos)])
                                (cond
                                  [(eq? c delimiter) (read-char in) null]
                                  [(eq? c #\&)
                                   (let ([entity (expand-entity (lex-entity in pos))])
                                     (if (pcdata? entity)
                                         (append (string->list (pcdata-string entity)) (read-more))
                                         ;; more here - do something with user defined entites
                                         (read-more)))]
                                  [else (read-char in) (cons c (read-more))]))))]
                          [else (lex-error in pos "attribute values must be in ''s or in \"\"s")])])
            (make-attribute start (pos) name value))))
      
      ;; skip-space : Input-port -> Void
      ;; deviation - should sometimes insist on at least one space
      (define (skip-space in)
        (let loop ()
          (let ([c (peek-char in)])
            (when (and (not (eof-object? c)) (char-whitespace? c))
              (read-char in)
              (loop)))))
      
      ;; lex-pcdata : Input-port (-> Location) -> Pcdata
      ;; deviation - disallow ]]> "for compatability" with SGML, sec 2.4 XML spec 
      (define (lex-pcdata in pos)
        (let ([start (pos)]
              [data (let loop ()
                      (let ([next (peek-char in)])
                        (cond
                          [(or (eof-object? next) (eq? next #\&) (eq? next #\<))
                           null]
                          [(and (char-whitespace? next) (collapse-whitespace))
                           (skip-space in)
                           (cons #\space (loop))]
                          [else (cons (read-char in) (loop))])))])
          (make-pcdata start
                       (pos)
                       (list->string data))))
      
      ;; lex-name : Input-port (-> Location) -> Symbol
      (define (lex-name in pos)
        (let ([c (read-char in)])
          (unless (name-start? c)
            (lex-error in pos "expected name, received ~a" c))
          (string->symbol
           (list->string
            (cons c (let lex-rest ()
                      (cond
                        [(name-char? (peek-char in))
                         (cons (read-char in) (lex-rest))]
                        [else null])))))))
      
      ;; skip-dtd : Input-port (-> Location) -> Void
      (define (skip-dtd in pos)
        (let skip ()
          (case (non-eof read-char in pos)
            [(#\') (read-until #\' in pos) (skip)]
            [(#\") (read-until #\" in pos) (skip)]
            [(#\<)
             (case (non-eof read-char in pos)
               [(#\!) (case (non-eof read-char in pos)
                        [(#\-) (read-char in) (lex-comment-contents in pos) (read-char in) (skip)]
                        [else (skip) (skip)])]
               [(#\?) (lex-pi-data in pos) (skip)]
               [else (skip) (skip)])]
            [(#\>) (void)]
            [else (skip)])))
      
      ;; name-start? : Char -> Bool
      (define (name-start? ch)
        (or (char-alphabetic? ch) 
            (eq? ch #\_)
            (eq? ch #\:)))
      
      ;; name-char? : Char -> Bool
      (define (name-char? ch)
        (or (name-start? ch)
            (char-numeric? ch)
            (eq? ch #\.)
            (eq? ch #\-)))
      
      ;; read-until : Char Input-port (-> Location) -> String
      ;; discards the stop character, too
      (define (read-until char in pos)
        (list->string
         (let read-more ()
           (let ([c (non-eof read-char in pos)])
             (cond
               [(eq? c char) null]
               [else (cons c (read-more))])))))
      
      ;; non-eof : (Input-port -> (U Char Eof)) Input-port (-> Location) -> Char
      (define (non-eof f in pos)
        (let ([c (f in)])
          (cond
            [(eof-object? c) (lex-error in pos "unexpected eof")]
            [else c])))
      
      ;; gen-read-until-string : String -> Input-port (-> Location) -> String
      ;; uses Knuth-Morris-Pratt from
      ;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
      ;; discards stop from input
      (define (gen-read-until-string stop)
        (let* ([len (string-length stop)]
               [prefix (make-vector len 0)]
               [fall-back
                (lambda (k c)
                  (let ([k (let loop ([k k])
                             (cond
                               [(and (> k 0) (not (eq? (string-ref stop k) c)))
                                (loop (vector-ref prefix (sub1 k)))]
                               [else k]))])
                    (if (eq? (string-ref stop k) c)
                        (add1 k)
                        k)))])
          (let init ([k 0] [q 1])
            (when (< q len)
              (let ([k (fall-back k (string-ref stop q))])
                (vector-set! prefix q k)
                (init k (add1 q)))))
          ;; (vector-ref prefix x) = the longest suffix that matches a prefix of stop
          (lambda (in pos)
            (list->string
             (let/ec out
               (let loop ([matched 0] [out out])
                 (let* ([c (non-eof read-char in pos)]
                        [matched (fall-back matched c)])
                   (cond
                     [(= matched len) (out null)]
                     [(zero? matched) (cons c (let/ec out (loop matched out)))]
                     [else (cons c (loop matched out))]))))))))
      
      ;; "-->" makes more sense, but "--" follows the spec.
      (define lex-comment-contents (gen-read-until-string "--"))
      (define lex-pi-data (gen-read-until-string "?>"))
      (define lex-cdata-contents (gen-read-until-string "]]>"))
      
      ;; positionify : Input-port -> Input-port (-> Location)
      (define (positionify in)
        (let ([line 1]
              [char 0]
              [offset 0])
          (values (make-input-port
                   (lambda ()
                     (set! char (add1 char))
                     (set! offset (add1 offset))
                     (let ([c (read-char in)])
                       (when (equal? c #\newline)
                         (set! line (+ line 1))
                         (set! char 0))
                       c))
                   (lambda () (char-ready? in))
                   (lambda () (peek-char in)))
                  (lambda () (make-location line char offset)))))
      
      ;; lex-error : Input-port String (-> Location) TST* -> alpha
      (define (lex-error in pos str . rest)
        (error 'lex-error "at position ~a: ~a" (format-source (pos))
               (apply format str rest)))
      
      
      ;; format-source : Location -> string
      ;; to format the source location for an error message
      (define (format-source loc)
        (if (location? loc)
            (format "~a.~a/~a" (location-line loc) (location-char loc) (location-offset loc))
            (format "~a" loc))))))