(module response-test-server mzscheme
  (require (lib "xml.ss" "xml")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           (lib "list.ss")
           "request-parsing.ss"
           "response.ss"
           "util.ss"
           "connection-manager.ss"
           "server.ss")

  (provide serve)

  (define myprint printf)

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
       [(and (not (null? dir-part))
             (string=? (car dir-part) "chunked"))
        (serve-chunked-file conn (apply build-path `(,(current-directory)
                                                     ,@(cdr dir-part)
                                                     ,f-part)))]
       [(and (not (null? dir-part))
             (string=? (car dir-part) "xexpr"))
        (serve-xexpr conn)]
       [(and (not (null? dir-part))
             (string=? (car dir-part) "bytes"))
        (serve-file-as-bytes conn (apply build-path `(,(current-directory)
                                                        ,@(cdr dir-part)
                                                        ,f-part)))]
       [(and (not (null? dir-part))
             (string=? (car dir-part) "full"))
        (serve-file-as-full conn (apply build-path `(,(current-directory)
                                                     ,@(cdr dir-part)
                                                     ,f-part)))]
       [(string=? f-part "")
        (serve-file conn (apply build-path `(,(current-directory) ,@dir-part
                                             "index.html")))]
       [else
        (serve-file conn (apply build-path `(,(current-directory) ,@dir-part
        ,f-part)))])))

  ;; serve-xexpr: connection -> void
  ;; serve up an x-expression
  (define (serve-xexpr conn)
    (output-response
     conn
     `(html
       (head (title "xexpr Page"))
       (body
        (h1 "xexpr Page")
        (p "This is my xexpr page.")))))

  ;; serve-file: connection path -> void
  ;; serve the given file if it exists
  (define (serve-file conn path)
    (cond
     [(file-exists? path)
      (output-file conn path 'get (get-mime-type path))]
     [else (do-404-error conn 'get)]))

  ;; serve-file-as-bytes: connection path -> void
  ;; bundle the file contents as a (listof string)-response and serve away
  (define (serve-file-as-bytes conn file-path)
    (output-response
     conn
     `(,(get-mime-type file-path) ,@(get-file-strings file-path))))

  ;; serve-file-as-full: connection path -> void
  ;; bundle the file contents as a response/full and serve away
  (define (serve-file-as-full conn file-path)
    (output-response
     conn
     (make-response/full
      200 "Okay" '()
      (file-or-directory-modify-seconds file-path)
      (get-mime-type file-path)
      (get-file-strings file-path))))

  ;; get-file-strings: string -> (listof bytes)
  (define (get-file-strings file-path)
    (call-with-input-file file-path
      (lambda (i-port)
        (let loop ([next (read-bytes 50 i-port)])
          (if (eof-object? next)
              '()
              (cons next (loop (read-bytes 50 i-port))))))))


  ;; serve-chunked-file: connection path -> void
  ;; serve the given file using chunked transfer coding
  (define (serve-chunked-file conn file-path)
    (myprint "serve-chunked-file~n")
    (cond
     [(file-exists? file-path)
      (output-response
       conn
       (make-response/incremental
        200 "Okay" '()
        (file-or-directory-modify-seconds file-path)
        (get-mime-type file-path)
        (chunked-file-generator file-path)))]
     [else (do-404-error conn 'get)]))

  ;; chunked-file-generator: path -> ((string* -> void) -> void)
  ;; a function to chop up a file into a (listof (listof string))
  ;; and feed it to a procedure
  (define (chunked-file-generator file-path)
    (lambda (blow-chunks)
      (call-with-input-file file-path
        (lambda (i-port)
          (let ([read-next
                 (lambda ()
                   (filter
                    bytes?
                    (list (read-bytes 50 i-port)
                          (read-bytes 50 i-port)
                          (read-bytes 50 i-port))))])
            (let loop ([next (read-next)])
              (unless (null? next)
                (apply blow-chunks next)
                (loop (read-next)))))))))

  ;; **************************************************
  ;; get-mime-type

  ;; get-mime-type: path -> string
  ;; this is not fully implemented and will be configurable
  (define get-mime-type
    (let ([file-suffix-regexp (regexp ".*\\.([^\\.]*$)")])
      (lambda (path)
        (let ([sffx (cadr (regexp-match file-suffix-regexp (path->string
                                                            path)))])
          (myprint "sffx = ~a~n" sffx)
          (cond
           [(string=? sffx "gif") "image/gif"]
           [else "text/html"])))))

  ;; do-404-error: connection symbol -> void
  ;; report that the file was not found
  (define (do-404-error conn method)
    (output-response/method
     conn
     (make-response/full
      404 "File not found" '() (current-seconds) "text/html"
      (list (xexpr->string
             `(html (head (title "File not found"))
                    (body
                     (p "The file referred to by this url was not
                         found"))))))
     method))

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
        (let-values ([(req close?) (read-request (connection-i-port conn))])
          (set-connection-close?! conn close?)
          (serve/url conn (request-uri req))
          close?))
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
