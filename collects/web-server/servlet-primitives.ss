(module servlet-primitives mzscheme
  (require "channel.ss"
           "configuration.ss"
           "configuration-structures.ss"
           "web-server.ss"
           "servlet-sig.ss"
           "servlet.ss"
           "internal-structs.ss"
           (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "browser.ss" "net")
           ; more here - use contracts when they support suitable error messages
           ;(lib "contracts.ss" "framework")
           (lib "error.ss" "htdp")
           (lib "unitsig.ss")
           )
  (provide servlet@)
  
  ; the unit doesn't contain much since it's better to start as few servers as possible
  (define servlet@
    (unit/sig servlet^
      (import)
  
      (define send/suspend the-send/suspend)
      (define send/finish the-send/finish)
      (define send/back the-send/back)
      (define send/forward the-send/forward)
      (define initial-request the-initial-request)
      (define adjust-timeout! the-adjust-timeout!)))
  
  ; : num -> void
  (define (the-adjust-timeout! n) (void))
  
  ; send/finish : response -> doesn't
  (define (the-send/finish page)
    (check-arg 'send/finish (valid-response? page) "response" "1st" page)
    (output-page page)
    (kill-thread (current-thread))
    (set! *page-channel* #f))
  
  ; : tst -> bool
  ; FIX - xexpr->xml is not a good way to check validity
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
  
  ; : instance -> doesn't
  (define resume-next-request
    (gen-resume-next-request void update-channel!))
  
  ; init-channel : -> void
  (define (init-channel)
    ((gen-send/suspend uri invoke-id instances void resume-next-request)
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
  
  ; : (str -> response) -> request
  (define the-send/suspend
    (let ((s/s (gen-send/suspend uri invoke-id instances output-page resume-next-request)))
      (lambda (k->page)
        (s/s (lambda (k-url)
               (let ([page (k->page k-url)])
                 (unless (valid-response? page)
                   (error 'send/suspend "expected <~a> as ~a argument, given a function that produced: ~e"
                          "a function that produces a response" "1st"
                          page))
                 page))))))

  ; : (response -> doesn't)
  (define (the-send/back page)
    (the-send/suspend (lambda (not-used-k-url) page)))

  ; : (str -> response) -> request
  (define (the-send/forward page-maker)
    ;(set-servlet-instance-cont-table!
    ; (hash-table-get invoke-id instances)
    ; (make-hash-table))
    ; FIX - this is wrong.  The hash table must be cleared or the reference in the server must be reset, not our reference.
    ; try (set-config-instances! the-config (make-hash-table))
    (set! instances (make-hash-table))
    (add-new-instance invoke-id instances)
    (the-send/suspend page-maker))
  
  (define the-initial-request
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
                         (lambda () (tcp-accept listener))
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
