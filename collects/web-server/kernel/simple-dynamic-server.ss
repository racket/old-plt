(module simple-dynamic-server mzscheme
  (require (lib "xml.ss" "xml")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           "servlet.ss"
           "request-parsing.ss"
           "response.ss"
           "resolver.ss"
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
      (let* ([vp (url->logical-path url)]
             [dir-part (logical-path-directory-part vp)]
             [f-part (logical-path-file-part vp)])
        (myprint "serve/request:~n     dir-part = ~s~n     f-part = ~s~n" dir-part f-part)
        (cond
         [(and (not (null? dir-part))
               (string=? (car dir-part) ".."))
          (report-error 404 conn (request-method req))]
         [(build-servlet-path req (current-directory) dir-part f-part)
          => (lambda (svt-path)
               (invoke-servlet svt-path (request-path-suffix req) conn req))]
         [else
          (report-error 404 conn (request-method req))]))))

  ;; build-servlet-path: req path (listof string) string -> path
  ;; Move up until the tree until you find
  ;; a file the tail becomes the servlet args
  (define (build-servlet-path req base-path dir-part f-part)
    (let build-it ([parts (if (string=? "" f-part)
                              dir-part
                              (append dir-part (list f-part)))]
                   [rest-parts '()])
      (if (null? parts)
          #f
          (let ([svt-path (apply build-path (cons base-path parts))])
            (cond
             [(file-exists? svt-path)
              (set-request-path-suffix! req rest-parts)
              svt-path]
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
           (invoke-servlet/k-ref conn req k-ref))]
     [(servlet-entry-id? tail-args)
      => (lambda (entry-sym)
           (invoke-servlet/entry servlet-path conn req entry-sym))]
     [(serializable-closure-id? tail-args)
      => (lambda (closure-id)
           (invoke-serializable-closure conn req closure-id))]
     [else (load-servlet/path servlet-path conn req)]))

  ;; servlet-entry-id?: (listof string) -> (union symbol #f)
  ;; determine if there is a servlet entry encoded in the tail args
  ;; TODO: this is quick and dirty, clean it up.
  (define (servlet-entry-id? tail-args)
    (and (not (null? tail-args))
         (string=? (car tail-args) "entry")
         (string->symbol (cadr tail-args))))

  ;; serializable-closure-id?: (listof string) -> (union symbol #f)
  ;; determine if there is a serializable-closure encoded in the tail args
  ;; TODO: this is quick and dirty, clean it up.
  (define (serializable-closure-id? tail-args)
    (and (not (null? tail-args))
         (string=? (car tail-args) "closure")
         (cadr tail-args)))

  ;; **************************************************
  ;; loading servlets

  ;; invoke-servlet/k: path connection request (list number number) -> void
  (define (invoke-servlet/k-ref servlet-path conn req k-ref)
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
        (let/cc suspend
          (thread-cell-set! current-servlet-context
                            (make-servlet-context
                             inst conn req
                             (lambda () (suspend #t))))
          (semaphore-wait (servlet-instance-mutex inst))
          ((hash-table-get k-table (cadr k-ref)
                           (lambda ()
                             (raise
                              (make-exn:servlet-continuation
                               "" (current-continuation-marks)))))
           req))
        (semaphore-post (servlet-instance-mutex inst)))))

  ;; invoke-servlet/entry: path connection request symbol -> void
  ;; make a new namespace, load the servlet in, dynamic-require the entry
  (define (invoke-servlet/entry servlet-path conn req entry-sym)
    (let/cc suspend
      (parameterize ([current-namespace (make-servlet-namespace)])
        (thread-cell-set!
         current-servlet-context
         (let ([inst (create-new-instance!)])
           (make-servlet-context inst conn req
                                 (lambda ()
                                   (semaphore-post (servlet-instance-mutex inst))
                                   (suspend #t)))))
        ((dynamic-require `(file ,(path->string servlet-path)) entry-sym) req))))

  ;; load-servlet/path: path connection request -> void
  (define (load-servlet/path servlet-path conn req)
    (invoke-servlet/entry servlet-path conn req 'start))

  ;; invoke-serializable-closure: connection request string -> void
  ;; make a new namespace, load the closure in, dynamic-require the apply
  (define (invoke-serializable-closure conn req closure-str)
    (invoke-servlet/entry (build-path (current-directory) "closures" closure-str)
                          conn req 'apply))

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
      (let ([inst (make-servlet-instance invoke-id (make-hash-table) 0
                                         (make-semaphore 0))])
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
