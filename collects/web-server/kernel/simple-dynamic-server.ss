(module simple-dynamic-server mzscheme
  (require (lib "xml.ss" "xml")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           (lib "url.ss" "net")
           "servlet.ss"
           "request-parsing.ss"
           "response.ss"
           "resolver.ss"
           "error-response.ss"
           "connection-manager.ss"
           "server.ss"
           "test-harness.ss")

  (provide serve)

  (define myprint printf)

  ;; for starters, we won't worry about headers or v-hosts
  ;; we'll just go directly off the URL

  ;; serve/request: connection request (listof resource-pair)-> void
  ;; create a logical path from the url-path and check for path chasing
  ;; apply the resource-map to the logical path and proceed based on
  ;; static vs. dynamic
  (define (serve/request conn req r-map)
    (let* ([v-path (url->logical-path (request-uri req))]
           [dir-part (logical-path-directory-part v-path)]
           [f-part (logical-path-file-part v-path)])
      (let-values ([(r-pair suff) (apply-resource-map r-map v-path)])
        (cond
         ;; check for path-chasing
         [(and (not (null? dir-part))
               (string=? (car dir-part) ".."))
          (report-error 404 conn (request-method req))]
         [(static-pair? r-pair) (serve-static-resource conn req r-pair suff)]
         [(dynamic-pair? r-pair) (serve-dynamic-resource conn req r-pair suff)]
         [else (report-error 404 conn (request-method req))]))))

  ;; serve-static-resource: connection request static-pair -> void
  ;; TODO: needs implementation
  (define (serve-static-resource conn req r-pair)
    (myprint "serve-static-resource~n")
    (error "not implemented"))

  (test "TODO: implement serve-static-resource"
        (lambda () #f))

  ;; serve-dynamic-resource: connection request dynamic-pair logical-path -> void
  ;; find the actual servlet in the filesystem, determine which suffix of
  ;; the url-path will be passed as arguments and invoke the servlet.
  (define (serve-dynamic-resource conn req d-pair suff)
    (let ([uri (request-uri req)])
      (cond
       [(continuation-url? uri)
        => (lambda (k-ref)
             (invoke-servlet-continuation conn req k-ref))]

       [else
        (let-values ([(svt-path path-prefix path-suffix)
                      (build-servlet-path d-pair suff)])
          (cond
           [svt-path
            (set-request-path-prefix! req path-prefix)
            (set-request-path-suffix! req path-suffix)
            (cond
             [(servlet-library? d-pair)
              (load-servlet-library svt-path path-suffix conn req)]
             [(persistent-continuation-url? uri)
              => (lambda (pk-ref)
                   (invoke-persistent-continuation svt-path conn req pk-ref))]
             [else
              (load-servlet svt-path conn req)])]
           [else
            (report-error 404 conn (request-method req))]))])))

  (define-struct (exn:servlet-instance exn) ())
  (define-struct (exn:servlet-continuation exn) ())
  (define-struct (exn:servlet-library exn) ())

  ;; **************************************************
  ;; loading servlets

  ;; load-servlet-library: path (listof string) connection request -> void
  ;; Find the serlvet-entry or use a default
  (define (load-servlet-library servlet-path path-suffix conn req)
    (myprint "load-servlet-library~n")
    (if (null? path-suffix)
        (load-servlet-library/entry servlet-path 'start conn req)
        (load-servlet-library/entry
         servlet-path (string->symbol (car path-suffix)) conn req)))

  ;; load-servlet-library/entry: path symbol connection request -> void
  ;; Make a namespace, require the servlet, lookup the entry and apply it
  ;; to the query args.
  (define (load-servlet-library/entry servlet-path entry-sym conn req)
    (myprint "load-servlet-library/entry entry-sym = ~s~n" entry-sym)
    (let ([args (map cdr (url-query (request-uri req)))])
      (let/cc suspend
        (parameterize ([current-namespace (make-servlet-namespace)])
          (thread-cell-set!
           current-servlet-context
           (let ([inst (create-new-instance!)])
             (make-servlet-context inst conn req
                                   (lambda ()
                                     (semaphore-post (servlet-instance-mutex inst))
                                     (suspend #t)))))
          (namespace-require `(file ,(path->string servlet-path)))
          (let ([entry-proc (namespace-variable-value entry-sym)])
            (if (servlet-entry? entry-proc)
                (apply entry-proc (cons req args))
                (raise
                 (make-exn:servlet-library "symbol is not a servlet entry"
                                           (current-continuation-marks)))))))))

  ;; load-servlet: path connection request -> void
  ;; servlet is just a simple module. namespace-require it and get on with life
  (define (load-servlet servlet-path conn req)
    (let/cc suspend
      (parameterize ([current-namespace (make-servlet-namespace)])
        (thread-cell-set!
         current-servlet-context
         (let ([inst (create-new-instance!)])
           (make-servlet-context inst conn req
                                 (lambda ()
                                   (semaphore-post (servlet-instance-mutex inst))
                                   (suspend #t)))))
        (namespace-require `(file ,(path->string servlet-path))))))

  ;; invoke-servlet-continuation: connection request (list number number) -> void
  ;; find the continuation and apply it to the request
  (define (invoke-servlet-continuation conn req k-ref)
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

  ;; invoke-persistent-continuation: connection request number -> void
  ;; find the persistent continuation and apply it to the request
  ;; Hmmm:
  ;;    persistent-continuations never expire, so we don't sort them by
  ;;    instance. Hence, pk-ref is just a number.
  (define (invoke-persistent-continuation servlet-path conn req pk-ref)
    (with-handlers ([exn:servlet-continuation?
                     (lambda (the-exn)
                       (report-error 404 conn (request-method req)))])

      ;; persistent-intance-table is a cache. Persistent instances
      ;; can be written to the filesystem too.
      (let ([pk (hash-table-get persistent-continuation-table pk-ref
                                (lambda ()
                                  (raise
                                   (make-exn:servlet-instance
                                    "" (current-continuation-marks)))))])
        (let/cc suspend
          (parameterize ([current-namespace (make-servlet-namespace)])
            (thread-cell-set! current-servlet-context
                              (let ([inst (create-new-instance!)])
                                (make-servlet-context
                                 inst conn req
                                 (lambda ()
                                   (semaphore-post (servlet-instance-mutex inst))
                                   (suspend #t)))))
            (namespace-require `(file ,(path->string servlet-path)))
            (eval `(,pk ,req)))))))

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
  (define persistent-continuation-table (make-hash-table))

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

      (define resource-map
        (list (dpair "/" "/test-root/")
              (slib "/lib/" "/test-root/")))

      ;; serve-connection: connection -> boolean
      ;; respond to the next request and return #t if the conneciton should be
      ;; closed
      (define (serve-connection conn)
        (let-values ([(req close?) (read-request (connection-i-port conn))])
          (serve/request conn req resource-map)
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
