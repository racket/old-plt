(module request-parsing mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "string.ss")
           (lib "list.ss")
           
           "internal-structs.ss"
           )
  
  (provide read-request-line
           read-headers
           get-host
           close-connection?
           read-bindings
           lowercase-symbol!)

;  (provide/contract
;   [read-request-line (input-port? . -> . symbol? bytes? bytes? bytes?)])

  ; lowercase-symbol! : (union string bytes) -> symbol
  (define (lowercase-symbol! s)
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s)))
  
  
  ;; **************************************************
  ;; read-request-line
  
  ; Method = (U 'get 'post 'head 'put 'delete 'trace)
  (define METHOD:REGEXP
    (byte-regexp #"^(GET|HEAD|POST|PUT|DELETE|TRACE) (.+) HTTP/([0-9]+)\\.([0-9]+)$"))
  
  (define (match-method x)
    (regexp-match METHOD:REGEXP x))
  ;:(define match-method (type: (str -> (union false (list str str str str str)))))
  
  
  ; read-request-line : iport -> symbol bytes bytes bytes
  ; to read in the first line of an http request, AKA the "request line"
  ; effect: in case of errors, complain [MF: where] and close the ports
  (define (read-request-line ip)
    (let ([line (read-bytes-line ip 'any)])
      (if (eof-object? line)
          (error 'read-request "http input closed abruptly")
          (cond
            [(match-method line)
             => (lambda (x)
                  (apply values (cons (lowercase-symbol! (cadr x)) (cddr x))))]
            [else (error 'read-request "malformed request ~a" line)]))))
  
  ;; **************************************************
  ;; read-headers
  
  ;(define COLON:REGEXP (regexp (format "^([^:]*):[ ~a]*(.*)" #\tab)))
  (define COLON:REGEXP (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)")))
  
  (define (match-colon s)
    (regexp-match COLON:REGEXP s))
  ;:(define match-colon (type: (str -> (union false (list str str str)))))  
  
  
  ; read-headers : iport -> (listof (cons symbol bytes))
  (define (read-headers in)
    (let read-header ()
      (let ([l (read-bytes-line in 'any)])
        (cond
          [(eof-object? l) null]
          [(zero? (bytes-length l)) null]
          [(match-colon l) =>
           (lambda (match)
             ; (cadr match) exists because COLON:REGEXP contains two (.)
             ; (caddr match) exists because COLON:REGEXP contains two (.)
             (cons (cons (lowercase-symbol! (cadr match))
                         (read-one-head in (caddr match)))
                   (read-header)))]
          [else (error 'read-headers "malformed header")]))))
  
  
  ; read-one-head : iport bytes -> bytes
  (define (read-one-head in rhs)
    (let ([c (peek-byte in)])
      (cond
        [(or (= c 32) (= c 9)) ;(or (eq? c #\space) (eq? c #\tab))
         
         ; (read-line in 'any) can't return eof
         ; because we just checked with peek-char
         ; Spidey: FLOW
         (read-one-head in (bytes-append rhs (read-bytes-line in 'any)))]
        [else rhs])))
  
  
  ;; **************************************************
  ;; get-host
  
  (define DEFAULT-HOST-NAME "<none>")
  
  ; get-host : Url (listof (cons Symbol bytes)) -> string
  ; host names are case insesitive---Internet RFC 1034
  ;; Notes (GregP):
  ;; 1. The host will either be part of the URL
  ;;    or will be identified via a header (RFC 2616 SECTION 5.1.2)
  
  (define (get-host uri headers)
    (let ([lower!
           (lambda (s)
             (string-lowercase! s)
             s)])
      (cond
        [(url-host uri) => lower!]
        [(assq 'host headers) =>
         (lambda (h)
           (lower! (bytes->string/utf-8 (cdr h))))]
        [else DEFAULT-HOST-NAME])))
  
  ;; **************************************************
  ;; close-connection
  
  ; close-connection? : table nat nat str str -> bool
  (define (close-connection? headers major minor client-ip host-ip)
    (or (< major 1)
        (and (= major 1) (= minor 0))
        (cond
          [(assq 'connection headers)
           => (lambda (x) (string-ci=? "close" (cdr x)))]
          [else #f])
        (msie-from-local-machine? headers client-ip host-ip)))
  
  
  ; : table str str -> bool
  ; to work around a bug in MSIE for documents < 265 bytes when connecting from the local
  ; machine.  The server could pad the response as MSIIS does, but closing the connection works, too.
  ; We do not check for version numbers since IE 6 under windows is 5.2 under macosX
  (define (msie-from-local-machine? headers client-ip host-ip)
    (and (string=? host-ip client-ip)
         (cond
           [(or (assq 'HTTP_USER_AGENT headers)
                (assq 'user-agent headers))
            => (lambda (client) (regexp-match MSIE-regexp (cdr client)))]
           [else #f])))
  (define MSIE-regexp (regexp "MSIE"))
  
  
  ;; **************************************************
  ;; read-mime-multipart
  
  ; read-mime-multipart : str iport -> (listof part)
  (define (read-mime-multipart boundary in)
    (let* ([boundary-len (string-length boundary)]
           [start-boundary (string-append "--" boundary)]
           [end-boundary (string-append start-boundary "--")])
      (let skip-preamble ()
        (let ([line (read-line in 'return-linefeed)])
          (cond
            [(string=? line start-boundary)
             (let read-parts ()
               (let ([headers (read-headers in)])
                 (let read-mime-part-body ([more-k (lambda (contents)
                                                     (cons (construct-mime-part
                                                            headers contents)
                                                           (read-parts)))]
                                           [end-k (lambda (contents)
                                                    (list (construct-mime-part
                                                           headers contents)))])
                   (let ([line (read-line in 'return-linefeed)])
                     (cond
                       [(string=? line start-boundary)
                        (more-k null)]
                       [(string=? line end-boundary)
                        (end-k null)]
                       [else (read-mime-part-body
                              (lambda (x) (more-k (cons line x)))
                              (lambda (x) (end-k (cons line x))))])))))]
            [(string=? line end-boundary) null]
            [else (skip-preamble)])))))
  
  ; more here - use structure, perhaps
  ; construct-mime-part : (listof header) (listof str) -> part
  (define (construct-mime-part headers body)
    (cons headers
          (cond
            [(null? body) null]
            [else (cons (car body)
                        (foldr (lambda (str acc)
                                 (list* CR-NL str acc))
                               null
                               (cdr body)))])))
  
  (define CR-NL (format "~a~a" #\return #\newline))
  
  ;; **************************************************
  ;; read-bindings
  
  (define INPUT-BUFFER-SIZE 4096)
  
  ;; connection symbol url host -> (union (listof (list symbol string)) #f)
  (define read-bindings
    (lambda (conn meth uri headers)
      (case meth
        [(get) (url-query uri)]
        [(post)
         (let ([content-type (assq 'content-type headers)])
           (cond
             [(and content-type (regexp-match FILE-FORM-REGEXP (cdr content-type)))
              => (lambda (content-boundary)
                   (map (lambda (part)
                          ; more here - better checks, avoid string-append
                          (cons (get-field-name (cdr (assq 'content-disposition (car part))))
                                (apply string-append (cdr part))))
                        (read-mime-multipart (cadr content-boundary) (connection-i-port conn))))]
             [else
              (let ([len-str (assq 'content-length headers)]
                    [in (connection-i-port conn)])
                (if len-str
                    (cond
                      [(string->number (cdr len-str))
                       => (lambda (len) (read-string len in))]
                      [else (error "Post request contained a non-numeric content-length")])
                    (apply string-append
                           (let read-to-eof ()
                             (let ([s (read-string INPUT-BUFFER-SIZE in)])
                               (if (eof-object? s)
                                   null
                                   (cons s (read-to-eof))))))))]))]
        [else (error "unsupported method" meth)])
      ))
  
  (define FILE-FORM-REGEXP (regexp "multipart/form-data; *boundary=(.*)"))
  
  ;; GregP: this is where I would get the filename out.
  ; get-field-name : str -> sym
  (define (get-field-name rhs)
    (let ([x (regexp-match "name=(\"([^\"]*)\"|([^ ;]*))" rhs)])
      (unless x
        (error 'get-field-name "Couldn't extract form field name for file upload from ~a" x))
      (string->symbol (or (caddr x) (cadddr x)))))
  
  )


