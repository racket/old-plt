(module servlet-primitives mzscheme
  (require "channel.ss"
           "configuration.ss"
           "parse-table.ss"
           "configuration-structures.ss"
           "web-server.ss"
           "servlet-sig.ss"
           (lib "url.ss" "net")
           (lib "sendurl.ss" "net"))
  (provide send/suspend
           send/finish
           initial-request) 
  
  ; send/finish : response -> doesn't
  (define (send/finish page)
    (output-page page)
    ; don't kill the server since it's still outputing the page
    ;(kill-thread (current-thread))
    (set! *page-channel* #f))
    
  ; *page-channel* : #f | channel
  (define *page-channel* #f)
  
  ; update-channel! : channel -> void
  (define (update-channel! x)
    (set! *page-channel* x))
  
  (define *last-page-sent* #f)
  (define *open-new-window* #t)
  
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
        (values (tcp-listen port)
                port))))
  
  (define instances (make-hash-table))
  (define uri (string->url (format "http://127.0.0.1:~a/servlets/" port)))
  (define invoke-id (string->symbol (symbol->string (gensym "id"))))
  
  ; send/suspend : (str -> page) -> (values Method Url Bindings Bindings)
  (define send/suspend
    (let ((s/s (gen-send/suspend uri invoke-id instances output-page void update-channel!)))
      (lambda (k->page) (s/s k->page))))
  
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
