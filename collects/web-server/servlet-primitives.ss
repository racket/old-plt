(module servlet-primitives mzscheme
  (require "channel.ss"
           (prefix config: "configuration.ss")
           "web-server.ss"
           "servlet-sig.ss"
           (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "process.ss"))
  (provide send/suspend
           send/finish
           initial-request) 
  
  ; send/finish : x-expression -> doesn't
  (define (send/finish page)
    (output-page page)
    ; don't kill the server since it's still outputing the page
    ;(kill-thread (current-thread))
    (set! *page-channel* #f))
  
;  (define void-output (make-output-port void void))
  (define void-output (open-output-file "/dev/null" 'append))
  
  ; open-in-browser : str -> void
  (define (open-in-browser url)
    (printf "Trying to open url: ~s~n" url)
    (or (system* netscape-path "-remote" (format "openURL(~a)" url))
        (begin (printf "Trying to start a new browser")
               '(let-values ([(p out in err) (subprocess #f #f #f netscape-path url)])
                 (close-input-port out)
                 (close-input-port err)
                 (close-output-port in))
               (let-values ([(p out in err) (subprocess void-output #f void-output netscape-path url)])
                 (close-output-port in)))))
  
  ; *page-channel* : #f | channel
  (define *page-channel* #f)
  
  ; update-channel! : channel -> void
  (define (update-channel! x)
    (set! *page-channel* x))
  
  ; output-page : page -> void
  (define (output-page page)
    (unless *page-channel*
      (init-channel))
    (channel-put *page-channel* page))
  
  ; input-port-closed? : iport -> bool
  ; there must be a better way
  (define (input-port-closed? in)
    (with-handlers ([exn:i/o:port:closed? (lambda (exn) #t)])
      (char-ready? in)
      #f))
  
  ; init-channel : -> void
  (define (init-channel)
    ((gen-send/suspend uri invoke-id instances void void update-channel!)
     (lambda (url) (open-in-browser url))))
  
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
  
  #|
  ; for the mac...
  (system "MSIE") 
  ; wait for some unspecified period of time ...
  (send-event "MSIE" "WWW!" "OURL" "http://www.brinckerhoff.org/")
  ; this stalls if MSIE isn't open yet.
  
  ; better -- from robby:
  (send-event "MACS" "GURL" "GURL" "http://www.brinckerhoff.org/")
  ; this asks the finder to open the selected browser and doesn't race.
  |#
  (define netscape-path (find-executable-path "netscape" #f))
  ;(define netscape-path "/usr/site/netscape-4.7.6/bin/netscape")
  
  (unless netscape-path
    (error 'netscape-path "Couldn't find Netscape."))
  
  (add-new-instance invoke-id instances)
  
  (thread (lambda ()
            (server-loop (current-custodian)
                         listener
                         (make-config config:virtual-hosts (make-hash-table)
                                      instances (make-hash-table))))))
