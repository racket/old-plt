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
      (output-file path (file-size path) 'get
                   (get-mime-type path) conn)]
     [else (do-404-error conn 'get)]))


  ;; **************************************************
  ;; get-mime-type

  ;; get-mime-type: path -> string
  ;; this is not fully implemented and will be configurable
  (define (get-mime-type ignored)
    "text/html")

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
      ;; respond to the next request and return #t if the conneciton should be
      ;; closed
      (define (serve-connection conn)
        (let ([req (read-request (connection-i-port conn))])
          (serve/url conn (request-uri req))
          (request-close? req)))
      ))


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
