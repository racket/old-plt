(module simple-static-server mzscheme
  (require (lib "xml.ss" "xml")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           "request-parsing.ss"
           "response-encoding.ss"
           "util.ss"
           "connection-manager.ss"
           "server.ss")

  (provide serve)

  ;; for starters, we won't worry about headers or v-hosts
  ;; we'll just go directly off the URL

  ;; serve/url: connection url -> void
  ;; transform the url to a virtual path, make sure it is safe and then serve
  ;; the file located relative to the current dirctory
  (define (serve/url conn url)
    (let* ([vp (url->virtual-path url)]
           [dir-part (virtual-path-directory-part vp)]
           [f-part (virtual-path-file-part vp)])
               dir-part f-part)
      (cond
       [(and (not (null? dir-part))
             (string=? (car dir-part) ".."))
        (do-404-error conn 'get)]
       [(string=? f-part "")
        (serve-file conn (apply build-path `(,(current-directory) ,@dir-part
                                             "index.html")))]
       [else
        (serve-file conn (apply build-path `(,(current-directory) ,@dir-part ,f-part)))])))

  ;; serve-file: connection path -> void
  ;; serve the given file if it exists
  (define (serve-file conn path)
    (cond
     [(file-exists? path)
      (output-file path (file-size path) 'get conn)]
     [else (do-404-error conn 'get)]))

  ;; do-404-error: connection symbol -> void
  ;; report that the file was not found
  (define (do-404-error conn method)
    (output-page/port
     conn
     (make-response/full
      404 "File not found" (current-seconds) "text/html" '()
      (if (eqv? method 'head) '()
          (list (xexpr->string
                 `(html (head (title "File not found"))
                        (body
                         (p "The file referred to by this url was not
                         found")))))))))

  (define my-config@
    (unit/sig server-config^
        (import)

      (define port 9000)
      (define max-waiting 20)
      (define listen-ip #f)
      (define initial-time-to-live 300)

      ;; serve-connection: connection -> boolean
      ;; respond to the next request and return #t if the connection should be kept open
      (define (serve-connection conn)
        (let ([ip (connection-i-port conn)])
          (let-values ([(method url major-version minor-version)
                        (read-request-line ip)])
            (let ([headers (read-headers ip)])
              (let-values ([(host-ip client-ip) (tcp-addresses ip)])
                (let ([close? (close-connection? headers major-version minor-version
                                                 client-ip host-ip)])
                  (set-connection-close?! conn close?)
                  (serve/url conn url)
                  close?))))))))

  (define-values/invoke-unit/sig
    server^
    (compound-unit/sig
      (import [TCP : net:tcp^])
      (link
       [CFG : server-config^ (my-config@)]
       [SRV : server^ (server@ CFG TCP)])
      (export (open SRV)))
    #f net:tcp^)


  )
