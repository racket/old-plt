(module servlet mzscheme
  (require (lib "url.ss" "net")
           (lib "list.ss")
           "response.ss"
           "request-parsing.ss")

  (provide send/back send/forward send/finish send/suspend
           current-servlet-context

           ;; URL manipulation
           embed-ids
           remove-ids
           embedded-ids?

           ;; data defs
           ;; TODO: close? is being kept in two places
           ;;       see if it can just be stored in the request
           ;;       and simplify the connection struct
           (struct servlet-context (instance connection request suspend))
           (struct servlet-instance (invoke-id k-table next-k-id mutex))

           ;; exceptions
           (struct exn:servlet ())
           )

  (define-struct servlet-instance (invoke-id k-table next-k-id mutex))
  (define-struct servlet-context (instance connection request suspend)
    (make-inspector))

  (define-struct (exn:servlet exn) ())

  ;; ********************************************************************************
  ;; The current-servlet-context parameter
  (define current-servlet-context (make-thread-cell #f))

  ;; clear-continuations! -> void
  ;; replace the k-table for the current servlet-instance
  (define (clear-continuations!)
    (set-servlet-instance-k-table!
     (servlet-context-instance
      (thread-cell-ref current-servlet-context))
     (make-hash-table)))

  ;; store-continuation!: continuation -> url-string
  ;; store a continuation in the k-table for the current servlet-instance
  (define (store-continuation! k)
    (let* ([ctxt  (thread-cell-ref current-servlet-context)]
           [inst (servlet-context-instance ctxt)]
           [next-k-id (servlet-instance-next-k-id inst)]
           [k-table (servlet-instance-k-table inst)])
      (set-servlet-instance-next-k-id! inst (add1 next-k-id))
      (hash-table-put! k-table next-k-id k)
      (embed-ids
       (servlet-instance-invoke-id inst) next-k-id
       (request-uri (servlet-context-request ctxt)))))

  ;; ********************************************************************************
  ;; Parameter Embedding

  (define URL-PARAMS:REGEXP (regexp "([^\\*]*)\\*(.*)"))

  (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))

  ;; embed-ids: number number url -> string
  ;; embedd the two numbers in a url
  (define (embed-ids invoke-id k-id in-url)
    (insert-param
     in-url
     (format "~a*~a" invoke-id k-id)))

  ;; embedded-ids?: url -> (union (list number number) #f)
  ;; determine if this url encodes a continuation and extract the instance id and
  ;; continuation id.
  (define (embedded-ids? a-url)
    (let ([str (url->param a-url)])
      (and str
           (map string->number (cdr (match-url-params str))))))

  ;; url->param: url -> (union string #f)
  (define (url->param a-url)
    (let ([l (filter path/param? (url-path a-url))])
      (and (not (null? l))
           (path/param-param (car l)))))

  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (replace-path
     (lambda (old-path)
       (if (null? old-path)
           (list (make-path/param "" new-param-str))
           (let* ([car-old-path (car old-path)])
             (cons (make-path/param (if (path/param? car-old-path)
                                        (path/param-path car-old-path)
                                        car-old-path)
                                    new-param-str)
                   (cdr old-path)))))
     in-url))

  ;; remove-ids: url -> string
  ;; replace all path/params with just the path/param-path part
  (define (remove-ids from-url)
    (replace-path
     (lambda (old-path)
       (map
        (lambda (path-elt)
          (if (path/param? path-elt)
              (path/param-path path-elt)
              path-elt))
        old-path))
     from-url))

  ;; replace-path: (url-path -> url-path) url -> url
  ;; replace the path part of a url
  (define (replace-path proc in-url)
    (let ([new-path (proc (url-path in-url))])
      (url->string
       (make-url
        (url-scheme in-url)
        (url-user in-url)
        (url-host in-url)
        (url-port in-url)
        new-path
        (url-query in-url)
        (url-fragment in-url)))))

  ;; **************************************************
  ;; send/*

  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (output-response
     (servlet-context-connection (thread-cell-ref current-servlet-context))
     resp)
    ((servlet-context-suspend (thread-cell-ref current-servlet-context))))

  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuations!)
    (send/back resp))

  ;; send/suspend: (url -> response) -> request
  ;; send a response and apply the continuation to the next request
  (define (send/suspend response-generator)
    (let/cc k
      (output-response
       (servlet-context-connection (thread-cell-ref current-servlet-context))
       (response-generator (store-continuation! k)))
      ((servlet-context-suspend (thread-cell-ref current-servlet-context)))))

  ;; send/forward: (url -> response) -> request
  ;; clear the continuation table, then behave like send/suspend
  (define (send/forward response-generator)
    (clear-continuations!)
    (send/suspend response-generator))

  )


