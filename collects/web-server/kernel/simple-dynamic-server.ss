(module simple-dynamic-server mzscheme
  (require (lib "xml.ss" "xml")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           "servlet.ss"
           "request-parsing.ss"
           "response.ss"
           "util.ss"
           "error-response.ss"
           "connection-manager.ss"
           "server.ss")

  (provide serve)

  (define myprint printf)

  ;; for starters, we won't worry about headers or v-hosts
  ;; we'll just go directly off the URL

  ;; serve/request: connection request -> void
  ;; transform the url to a virtual path, make sure it is safe and then serve
  ;; the file located relative to the current dirctory
  (define (serve/request conn req)
    (let ([url (request-uri req)])
      (let* ([vp (url->virtual-path url)]
             [dir-part (virtual-path-directory-part vp)]
             [f-part (virtual-path-file-part vp)])
        (cond
         [(and (not (null? dir-part))
               (string=? (car dir-part) ".."))
          (report-error 404 conn (request-method req))]
         [(build-servlet-path (current-directory) dir-part f-part)
          => (lambda (svt-path-stuff)
               (invoke-servlet (car svt-path-stuff) (cadr svt-path-stuff) conn req))]
         [else
          (report-error 404 conn (request-method req))]))))

  ;; build-servlet-path: path (listof string) string -> (union #f (list path (listof string))
  ;; Move up until the tree until you find
  ;; a file the tail becomes the servlet args
  (define (build-servlet-path base-path dir-part f-part)
    (let build-it ([parts (if (string=? "" f-part)
                              dir-part
                              (append dir-part (list f-part)))]
                   [rest-parts '()])
      (if (null? parts)
          #f
          (let ([svt-path (apply build-path (cons base-path parts))])
            (cond
             [(file-exists? svt-path)
              (list svt-path rest-parts)]
             [else
              (call-with-values
               (lambda () (shuffle-lists parts rest-parts))
               build-it)])))))

  ;; shuffle-lists: (listof x) (listof x) -> (listof x) (listof x)
  ;; move the last car of one list to the front of the other and return the
  ;; resulting new lists
  (define (shuffle-lists l1 l2)
    (cond
     [(null? l1) (values l1 l2)]
     [(null? (cdr l1)) (values '() (cons (car l1) l2))]
     [else
      (let-values ([(new-l1 new-l2) (shuffle-lists (cdr l1) l2)])
        (values
         (cons (car l1) new-l1)
         new-l2))]))


  (define-struct (exn:servlet-instance exn) ())
  (define-struct (exn:servlet-continuation exn) ())

  ;; invoke-servlet: path (listof string) connection request -> void
  ;; determine if it is the start of a servlet or continuation of a servlet
  (define (invoke-servlet servlet-path tail-args conn req)
    (cond
     [(embedded-ids? (request-uri req))
      => (lambda (k-ref)
           (with-handlers ([exn:servlet-instance?
                            (lambda (the-exn)
                              (report-error 404 conn (request-method req)))]
                           [exn:servlet-continuation?
                            (lambda (the-exn)
                              (report-error 404 conn (request-method req)))])
            (let* ([inst (hash-table-get instance-table (car k-ref)
                                         (lambda ()
                                           (raise
                                            (make-exn:servlet-instance
                                             "" (current-continuation-marks)))))]
                   [k-table
                    (servlet-instance-k-table inst)])
              (let/ec suspend
                (thread-cell-set! current-servlet-context
                                  (make-servlet-context
                                   inst conn req
                                   (lambda () (suspend #t))))
                ((hash-table-get k-table (cadr k-ref)
                                 (lambda ()
                                   (raise
                                    (make-exn:servlet-continuation
                                     "" (current-continuation-marks)))))
                 req)))))]
     [else (load-servlet/path servlet-path conn req)]))

  ;; **************************************************
  ;; loading servlets

  ;; load-servlet/path: path connection -> void
  ;; make a new namespace, load the servlet in and let 'r rip!
  (define (load-servlet/path servlet-path conn req)
    (let/ec suspend
      (parameterize ([current-namespace (make-servlet-namespace)])
        (thread-cell-set! current-servlet-context
                          (make-servlet-context (create-new-instance!)
                                                conn req
                                                (lambda () (suspend #t))))
        (namespace-require `(file ,(path->string servlet-path))))))

  ;; servlet-import-modules is a (listof symbol)
  ;; use the current-module-name resolver to get the symbols for all the
  ;; modules in a list of module specifications.
  ;; a module specification is something like (lib "foo.ss" "foo-collection")
  (define servlet-import-modules
    (let ([get-name
           (lambda (m-spec)
             (if (symbol? m-spec)
                 m-spec
                 ((current-module-name-resolver) m-spec #f #f)))])
      (map
       get-name
       '(mzscheme
         "servlet.ss"))))

  ;; make-servlet-namespace: -> namespace
  ;; create a namespace with the servlet-import-modules attached
  ;; these modules are attached from the server-namespace and thus
  ;; should share state with the server
  (define (make-servlet-namespace)
    (let ([server-namespace (current-namespace)]
          [servlet-namespace (make-namespace)])
      (parameterize ([current-namespace servlet-namespace])
        (for-each
         (lambda (name)
           (namespace-attach-module server-namespace name))
         servlet-import-modules)
        servlet-namespace)))

  (define instance-table (make-hash-table))
  (define next-invoke-id 0)

  ;; create-new-instance! args -> servlet-instance
  (define (create-new-instance!)
    (let ([invoke-id next-invoke-id])
      (set! next-invoke-id (add1 next-invoke-id))
      (let ([inst (make-servlet-instance invoke-id (make-hash-table) 0)])
        (hash-table-put! instance-table invoke-id inst)
        inst)))

  ;; **************************************************
  ;; get-mime-type

  ;; get-mime-type: path -> string
  ;; this is not fully implemented and will be configurable
  (define (get-mime-type ignored)
    "text/html")

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
          (serve/request conn req)
          (set-connection-close?! conn close?)
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
