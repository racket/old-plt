(module dynamic-server mzscheme
  (require (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           "util.ss"
           "request-parsing.ss"
           "response-encoding.ss"
           "server-kernel-structs.ss"
           "servlet.ss")
  (provide meta-serve)

  (define myprint printf)
  (define timeouts-file-base 60)
  (define timeouts-file-per-byte 60)
  (define servlet-path-base (current-directory))
  (define instance-table (make-hash-table))
  (define next-invoke-id 0)

  (define (request->method req)
    (request-line-method (http-request-line req)))

  ;; meta-serve: connection request-line -> boolean
  ;; read the request headers
  ;; determine whether this is the last response and respond
  (define (meta-serve conn req-line)
    (let* ([ip (connection-i-port conn)]
           [headers (read-headers ip)]
           [uri (string->url
                 (bytes->string/utf-8 (request-line-uri-string req-line)))])
      (let-values ([(host-ip client-ip) (tcp-addresses ip)])
        (let ([close? (close-connection?
                       headers
                       (string->number
                        (bytes->string/utf-8 (request-line-major-version req-line)))
                       (string->number
                        (bytes->string/utf-8 (request-line-minor-version req-line)))
                       client-ip host-ip)])
          (set-connection-close?! conn close?)
          (serve-http-response conn (make-http-request req-line uri headers close?))
          close?))))
     
  ;; serve-http-response: connection http-request -> void
  ;; locate a meta-resource, load it and respond
  (define (serve-http-response conn req)
    (let ([virtual-servlet-path (url->virtual-path (http-request-uri req))])

      ;; TODO need an exception and good error message for this
      (let ([dir-part (virtual-path-directory-part virtual-servlet-path)]
            [file-part (virtual-path-file-part virtual-servlet-path)])
        (when (and (not (null? dir-part))
                   (string=? ".." (car dir-part)))
              (error "bad path, tried to chase \"..\" above root"))

        (myprint "dir-part = ~a     file-part = ~a~n" dir-part file-part)
        (let ([real-servlet-path
               (apply build-path `(,servlet-path-base
                                   ,@dir-part
                                   ,@(if (string=? "" file-part) '()
                                         (list file-part))))])
          (invoke-servlet real-servlet-path conn req)))))

  (define-struct (exn:servlet-instance exn) ())
  (define-struct (exn:servlet-continuation exn) ())

  ;; invoke-servlet: path connection request -> void
  ;; determine if it is the start of a servlet or continuation of a servlet
  (define (invoke-servlet servlet-path conn req)
    (cond
     [(embedded-ids? (http-request-uri req))
      => (lambda (k-ref)
           (with-handlers ([exn:servlet-instance?
                            (lambda (the-exn)
                              (instance-not-found conn req))]
                           [exn:servlet-continuation?
                            (lambda (the-exn)
                              (continuation-not-found conn req))])
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

  ;; load-servlet/path path connection -> void
  ;; for starters, let's keep this as simple as possible...:
  (define (load-servlet/path servlet-path conn req)
    (myprint "servlet-path = ~a~n" servlet-path)
    (let/ec suspend
     (parameterize ([current-namespace (make-servlet-namespace)])
      (thread-cell-set! current-servlet-context
                        (make-servlet-context (create-new-instance!)
                                           conn req
                                           (lambda () (suspend #t)))
      (namespace-require `(file ,(path->string servlet-path)))))))

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

  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; helpers

  ;; create-new-instance! args -> servlet-instance
  (define (create-new-instance!)
    (let ([invoke-id next-invoke-id])
      (set! next-invoke-id (add1 next-invoke-id))
      (let ([inst (make-servlet-instance invoke-id (make-hash-table) 0)])
        (hash-table-put! instance-table invoke-id inst)
        inst)))

  
  ;; continuation-not-found: connection request -> void
  (define (continuation-not-found conn req)
    (output-page/port
     conn
     (make-response/full
      200 "Timeout" (current-seconds) "text/html" '()
      (if (eq? (request->method req) 'head) '()
          (list (xexpr->string
                 `(html (head (title "Timeout"))
                        (body
                         (p "The transaction referred to by this url is no longer active."
                            " Please "
                            (a ([href ,(remove-ids (http-request-uri req)  #f)])
                               "restart")
                            " the transaction.")))))))))

  ;; instance-not-found: connection request -> void
  (define (instance-not-found conn req)
    (output-page/port
     conn
     (make-response/full
      404 "Servlet not found" (current-seconds) "text/html" '()
      (if (eq? (request->method req) 'head) '()
          (list (xexpr->string
                 `(html (head (title "File not found"))
                        (body
                         (p "The file referred to by this url was not found")))))))))
  )
