#cs
(module proxy-server mzscheme 
  
  ;; WARNING: I haven't run this through proxy-server test since eliminating the custodian
  
  ;; the proxy server creates a tcp listener and for each connection 
  ;; registers a proxy-player (if possible) with the given administrator 
  ;; the result is a custodian, which can shut down all threads and connections
  
  (require
   "if.scm" "proxy-player.scm" "contract.scm" "proxy-auxs.scm" 
   (lib "class.ss")
   (lib "date.ss")
   (lib "etc.ss")
   (lib "thread.ss")
   (lib "xml.ss" "xml"))
  
  (provide 
   server   ;; admin<%> [(String -> Boolean)] -> Listof[(list String Follower)]
   ;; POST: the resulting list is at least two elements long 
   ;; PRE:  the given predicate produces true at least twice 
   ;; ASSUME: someone has set up a custodian
   
   play-list% ;; 
   )
  
  (define play-list%
    (class object% 
      (super-new)
      (define/public (legal? n) #t)
      (define/public (registered! n) (void))))

  (define server 
    (opt-lambda (admin [play-list (new play-list%)])
      (define listen (tcp-listen PORT 30 #t #f))
      (define tcp-cu (make-custodian))
      (define today  (date->string (seconds->date (current-seconds)) #t))
      (define _players '())
      (define (action)
        (let register-loop ()
          (define-values (name color)
            (parameterize ([current-custodian tcp-cu])
              (define-values (in out) (tcp-accept listen))
              (define (handler x)
                (close-input-port in)
                (close-output-port out)
                (printf "~a~n" (if (exn? x) (exn-message x) (contract-msg x)))
                (values 'skip 'skip))
              (with-handlers ([contract? handler][exn:fail? handler])
                (define name 
                  (timed-action (lambda () (create-proxy-player in out)) MAX-REGISTRATION-TIME))
                (if (send play-list legal? name)
                    (let ()
                      (define pp 
                        (new proxy-player% [input in][output out][name name]))
                      (define color (send admin register pp))
                      (with-handlers 
                          ([exn? 
                            (lambda (x)
                              (raise (make-exn (string-append name " : " (exn-message x)) '_)))])
                        ([make-return out] color)
                        (values name color)))
                    (values 'skip 'skip)))))
          (cond
            [(boolean? color) _players]
            [(eq? 'skip name) (register-loop)]
            [else (set! _players (cons (list name color) _players))
                  (send play-list registered! name)
                  (register-loop)])))
      (printf "ready to register players @ ~a~n" today)
      (let r ()
        (define x (with-handlers ([contract? (lambda (x) 
                                               (printf "server registration times out~n")
                                               _players)])
                    (timed-action action (REG-WAIT) void)))
        (if (> (length x) 1) x (r)))))
  
  (define REGISTRATION "Registration.txt")
  
  ;; IPost OPort -> String 
  ;; create a thunk that creates a proxy player from a registration message 
  (define (create-proxy-player in out)
    (define (sequence-error register)
      (sequence/violation
       (format "expected a registration message, received ~e" register)))
    (define listen
      (make-listen in void (lambda () (sequence-error 'eof)) sequence-error))
    (define (cb m args)
      (if (and (eq? 'register m) (pair? args))
          (raise (car args))
          (sequence-error `(,m . ,args))))
    (with-handlers ([exn:fail? sequence-error] [string? (lambda (x) x)])
      (listen cb))))
