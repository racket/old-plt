(module servlet-primitives mzscheme
  (require "channel.ss"
           "configuration.ss"
           "parse-table.ss"
           "configuration-structures.ss"
           "web-server.ss"
           "servlet-sig.ss"
           (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "browser.ss" "net")
           ; more here - use contracts when they support suitable error messages
           ;(lib "contracts.ss" "framework")
           (lib "error.ss" "htdp")
           )
  (provide send/suspend
           send/finish
           initial-request)
  
  ; send/finish : response -> doesn't
  (define (send/finish page)
    (check-arg 'send/finish (valid-response? page) "response" "1st" page)
    (output-page page)
    (kill-thread (current-thread))
    (set! *page-channel* #f))
  
  ; : tst -> bool
  (define (valid-response? page)
    (with-handlers ([void (lambda (exn) #f)])
      (or (response/full? page)
          ; this could fail for dotted lists - rewrite andmap
          (and (pair? page) (pair? (cdr page)) (andmap string? page)) 
          (and (xexpr->xml page) #t))))
  
  ; *page-channel* : #f | channel
  (define *page-channel* #f)
  
  ; update-channel! : channel -> void
  (define (update-channel! x)
    (set! *page-channel* x))
  
  (define *last-page-sent* #f)
  ;(define *open-new-window* #t)
  ; always re-use an exisiting window.
  (define *open-new-window* #f)
  
  ; output-page : page -> void
  (define (output-page page)
    (set! *last-page-sent* page)
    ;(unless *page-channel*
    ;  (init-channel))
    (init-channel)
    (channel-put *page-channel* page))
  
  ; init-channel : -> void
  (define (init-channel)
    ((gen-send/suspend uri invoke-id instances void void update-channel!)
     (lambda (url)
       (send-url url *open-new-window*)
       (set! *open-new-window* #f))))
  
  (define-values (listener port)
    (let loop ([port 8000])
      (with-handlers ([void (lambda (exn) (loop (add1 port)))])
        (values (tcp-listen port 10 #f "127.0.0.1")
                port))))
  
  (define instances (make-hash-table))
  (define uri (string->url (format "http://127.0.0.1:~a/servlets/" port)))
  (define invoke-id (string->symbol (symbol->string (gensym "id"))))
  
  ; send/suspend : (str -> page) -> (values Method Url Bindings Bindings)
  (define send/suspend
    (let ((s/s (gen-send/suspend uri invoke-id instances output-page void update-channel!)))
      (lambda (k->page)
        (s/s (lambda (k-url)
               (let ([page (k->page k-url)])
                 (unless (valid-response? page)
                   (error 'send/suspend "expected <~a> as ~a argument, given a function that produced: ~e"
                          "a function that produces a response" "1st"
                          page))
                 page))))))
  
  (define initial-request
    (make-request 'post uri null null "127.0.0.1" "127.0.0.1"))
  
  (add-new-instance invoke-id instances)
  
  ; override some configuration options
  (define the-configuration
    (load-developer-configuration default-configuration-table-path))
  
  (define big-timeout (* 24 60 60))
  (define the-config
    (make-config (configuration-virtual-hosts the-configuration)
                 (make-hash-table)
                 instances (make-hash-table)))
  
  (thread (lambda ()
            (server-loop (current-custodian)
                         listener
                         the-config
                         big-timeout
                         (lambda ()
                           ; having a failure thunk without a success thunk makes the control flow
                           ; strange -- output-page assumes everything works, but if it doesn't
                           ; we have to go back and try again.  It's rather stateful.  There's
                           ; probably a more principled way to do this.
                           (printf "Restarting STOPPED browser...~n")
                           (init-channel)
                           (channel-put *page-channel* *last-page-sent*))))))
