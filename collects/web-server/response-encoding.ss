(module response-encoding mzscheme
  (require (lib "list.ss")
           (lib "xml.ss" "xml")
           
           "internal-structs.ss"
           
           "servlet-tables.ss"
           "servlet-sig.ss")
  
  
  (provide output-headers
           report-error
           decapitate
           output-page/port
           )
  
  ;; **************************************************
  ;; output-headers:
  
  ; output-headers : connection Nat String (listof (listof String)) Nat String -> Void
  (define output-headers
    (case-lambda
      [(conn code message extras seconds mime)
       (let ([out (connection-o-port conn)])
         (for-each (lambda (line)
                     (for-each (lambda (word) (display word out)) line)
                     (display #\return out)
                     (newline out))
                   (list* `("HTTP/1.1 " ,code " " ,message)
                          `("Date: " ,(seconds->gmt-string (current-seconds)))
                          `("Last-Modified: " ,(seconds->gmt-string seconds))
                          `("Server: PLT Scheme")
                          `("Content-type: " ,mime)
                          ; more here - consider removing Connection fields from extras or raising an error
                          (if (connection-close? conn)
                              (cons `("Connection: close") extras)
                              extras)))
         (display #\return out)
         (newline out))]
      [(conn code message extras)
       (output-headers conn code message extras (current-seconds) TEXT/HTML-MIME-TYPE)]
      [(conn code message)
       (output-headers conn code message '() (current-seconds) TEXT/HTML-MIME-TYPE)]))

  
  ; seconds->gmt-string : Nat -> String
  ; format is rfc1123 compliant according to rfc2068 (http/1.1)
  (define (seconds->gmt-string s)
    (let* ([local-date (seconds->date s)]
           [date (seconds->date (- s
                                   (date-time-zone-offset local-date)
                                   (if (date-dst? local-date) 3600 0)))])
      (format "~a, ~a ~a ~a ~a:~a:~a GMT"
              (vector-ref DAYS (date-week-day date))
              (two-digits (date-day date))
              (vector-ref MONTHS (sub1 (date-month date)))
              (date-year date)
              (two-digits (date-hour date))
              (two-digits (date-minute date))
              (two-digits (date-second date)))))
  
  ; two-digits : num -> str
  (define (two-digits n)
    (let ([str (number->string n)])
      (if (< n 10) (string-append "0" str) str)))
  
  (define MONTHS
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  
  (define DAYS
    #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  
  ;; **************************************************
  ;; report-error
  
  ; report-error : connection method response -> void
  (define (report-error conn method response)
    (output-page/port conn (decapitate method response)))
  
  ; decapitate : method response -> response
  ; to remove the body if the method is 'head
  (define (decapitate method response)
    (if (eq? method 'head)
        (cond
          [(response/full? response)
           (make-response/full (response/full-code response)
                               (response/full-message response)
                               (response/full-seconds response)
                               (response/full-mime response)
                               (response/full-extras response)
                               (response/full-body response))]
          [else (make-response/full 200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE null null)])
        response))
  
  ;; **************************************************
  ;; output-page/port
  
  ; output-page/port : connection response -> void
  (define (output-page/port conn page)
    (let ([out (connection-o-port conn)]
          [close (connection-close? conn)])
      ; double check what happens on erronious servlet output
      ; it should output an error for this response
      (cond
        [(response/full? page)
         (cond
           [(response/incremental? page)
            (output-headers/response/full
             conn page
             (if close
                 null
                 `(("Transfer-Encoding: chunked")
                   . ,(map (lambda (x) (list (symbol->string (car x)) ": " (cdr x)))
                           (response/full-extras page)))))
            (if close
                ; WARNING: This is unreliable because the client can not distinguish between
                ; a dropped connection and the end of the file.  This is an inherit limitation
                ; of HTTP/1.0.  Other cases where we close the connection correspond to work arounds
                ; for buggy IE versions, at least some of which don't support chunked either.
                ((response/full-body page)
                 (lambda chunks
                   (for-each (lambda (chunk) (display chunk out)) chunks)))
                (begin
                  ((response/full-body page)
                   (lambda chunks
                     (fprintf out "~x\r\n" (foldl (lambda (c acc) (+ (string-length c) acc)) 0 chunks))
                     (for-each (lambda (chunk) (display chunk out)) chunks)
                     (fprintf out "\r\n")))
                  ; one \r\n ends the last (empty) chunk and the second \r\n ends the (non-existant) trailers
                  (fprintf out "0\r\n\r\n")))]
           [else
            (output-headers/response/full
             conn page
             `(("Content-length: " ,(apply + (map string-length (response/full-body page))))
               . ,(map (lambda (x) (list (symbol->string (car x)) ": " (cdr x)))
                       (response/full-extras page))))
            (for-each (lambda (str) (display str out))
                      (response/full-body page))])]
        [(and (pair? page) (string? (car page)))
         (output-headers conn 200 "Okay"
                         `(("Content-length: " ,(apply + (map string-length (cdr page)))))
                         (current-seconds) (car page))
         (for-each (lambda (str) (display str out))
                   (cdr page))]
        [else
         (let ([str (with-handlers ([void (lambda (exn)
                                            (if (exn? exn)
                                                (exn-message exn)
                                                (format "~s" exn)))])
                      (xexpr->string page))])
           (output-headers conn 200 "Okay"
                           `(("Content-length: " ,(add1 (string-length str)))))
           (display str out) ; the newline is for an IE 5.5 bug workaround
           (newline out))])))
  
  ;; output-headers/response/full: connection response/full extras ->
  ;; see def of output-headers for datadef of extras
  ;; unpack response/full and call output-headers
  (define (output-headers/response/full conn r/full extras)
    (output-headers conn
                    (response/full-code r/full)
                    (response/full-message r/full)
                    extras
                    (response/full-seconds r/full)
                    (response/full-mime r/full)))
  
 )