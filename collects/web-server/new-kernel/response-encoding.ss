(module response-encoding mzscheme
  (require (lib "thread.ss")
           "server-kernel-structs.ss")
  (provide output-headers
           output-file
           get-mime-type)
  
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
  
  (define TEXT/HTML-MIME-TYPE "text/html")
  
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
  ;; output-file
  
  ; output-file : path number symbol connection -> void
  ; to serve out the file
  (define (output-file path size method conn)
    (output-headers conn 200 "Okay"
                    `(("Content-length: " ,size))
                    (file-or-directory-modify-seconds path)
                    (get-mime-type path))
    (when (eq? method 'get)
      (call-with-input-file path (lambda (in) (copy-port in (connection-o-port conn))))))
  
  ;; **************************************************
  ;; get-mime-type
  
  ;; not implemented !!
  (define (get-mime-type ignored)
    "text/html")
  )
