(module request-parsing mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           "util.ss")

  (provide/contract
   [close-connection? (list? number? number? string? string? . -> . boolean?)]
   [read-request-line ((input-port?) . ->* . (symbol? url? number? number?))]
   [read-headers (input-port? . -> . list?)])
  
  ;; **************************************************
  ;; close-connection?
  
  ; close-connection? : (listof (cons symbol bytes)) number number string string -> boolean
  ; determine if this connection should be closed after serving the response
  (define (close-connection? headers major minor client-ip host-ip)
    (or (< major 1)
        (and (= major 1) (= minor 0))
        (cond
          [(assq 'connection headers)
           => (lambda (x) (string-ci=? "close" (bytes->string/utf-8 (cdr x))))]
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
  ;; read-request-line
  
  ; Method = (U 'get 'post 'head 'put 'delete 'trace)
  (define METHOD:REGEXP
    (byte-regexp #"^(GET|HEAD|POST|PUT|DELETE|TRACE) (.+) HTTP/([0-9]+)\\.([0-9]+)$"))
  
  (define (match-method x)
    (regexp-match METHOD:REGEXP x))
  ;:(define match-method (type: (str -> (union false (list str str str str str)))))
  
  
  ; read-request-line : iport -> symbol url number number
  ; to read in the first line of an http request, AKA the "request line"
  ; effect: in case of errors, complain [MF: where] and close the ports
  (define (read-request-line ip)
    (let ([line (read-bytes-line ip 'any)])
      (if (eof-object? line)
          (error 'read-request "http input closed abruptly")
          (cond
            [(match-method line)
             => (lambda (x)
                  (values
                   (lowercase-symbol! (list-ref x 1))
                   (string->url (bytes->string/utf-8 (list-ref x 2)))
                   (string->number (bytes->string/utf-8 (list-ref x 3)))
                   (string->number (bytes->string/utf-8 (list-ref x 4)))))]
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
  )
