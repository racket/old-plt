(module proxy-logging mzscheme
  
  ;; auxililary functions for logging exchanges to and from proxy-players 

  (require "if.scm" "tiles.scm" (lib "date.ss") (lib "pretty.ss") (lib "class.ss"))
  
  (provide 
   start-logging ;; -> Void
   ;; set up a directory for logging a game 
   
   create-log-file ;; String [Symbol] -> OPort 
   ;; create a log file for the raw data from a specific player 
   ;; this is only useful in the case of a time-out failure 
   
   log-messages ;; -> (String (X ... -> Y) -> (X ... -> Y))
   ;; create a logging version of the function 
   ;; this is for a working player
   )
  
  ;; --- minutes of a game --- 
  
  (define minutes-dir "tmp")
  
  (define (start-logging) 
    (define tmp (build-path "tmp" (format "tmp~a" (now 'date))))
    (make-directory tmp)
    (set! minutes-dir tmp))
  
  (define create-log-file
    (case-lambda 
      [(filename mode) 
       (define fn  (substring filename 0 (min (string-length filename) 100)))
       (define pth (build-path minutes-dir fn))
       (open-output-file pth mode)]
      [(filename) (create-log-file filename 'append)]))
  
  (define (log-messages player)
    ;(define port (create-log-file (string-append "good-" player)))
    (lambda (tag f)
      (define (flog . x)
        (fprintf port "~n;; --- time: ~s ~a~n" (now) tag)
        (pretty-print (map plain x) port)
        (apply f x))
      flog)
    void)
  
  ;; Sexp -> Sexp
  ;; remove procedures, replace tiles with flat values 
  (define (plain x)
    (cond
      [(null? x) '()]
      [(tile? x) `(make-tile ,(send x get-index) 
                             ,(send x get-x)
                             ,(send x get-y)
                             ,(send x get-o))]
      [(pair? x) (cons (plain (car x)) (plain (cdr x)))]
      [(procedure? x) '<should-be-a-take-turn>]
      [else x #;(error 'plain "shouldn't happen in logging: ~e" x)]))
  
  ;; [Any] -> String 
  ;; produce current date and time as string, with _ for spaces if (pair? x)
  (define (now . x)
    (define cs (current-seconds))
    (define dt (seconds->date cs))
    (define st (date->string dt #t))
    (if (pair? x)
        (regexp-replace* 
         ":" (regexp-replace* "," (regexp-replace* " " st "_") "_") "_")
        st))

  )