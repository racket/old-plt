(module servlet-tables mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "list.ss"))
  (provide (struct exn:servlet-instance ())
           (struct exn:servlet-continuation ())
           (struct servlet-context (instance connection request suspend))
           (struct servlet-instance (invoke-id k-table next-k-id mutex))
           current-servlet-context)

  (define-struct servlet-context (instance connection request suspend))
  (define-struct servlet-instance (invoke-id k-table next-k-id mutex))

  (provide/contract
   [continuation-url? (url? . -> . (union boolean? (list/p symbol? number?)))]
   [embed-ids (symbol? number? url? . -> . string?)]
   [create-new-instance! (hash-table? symbol? . -> . servlet-instance?)]
   )

  ;; The current-servlet-context parameter
  (define current-servlet-context (make-thread-cell #f))

  ;; not found in the instance table
  (define-struct (exn:servlet-instance exn) ())
  ;; not found in the continuatin table
  (define-struct (exn:servlet-continuation exn) ())


  ;; create-new-instance! args -> servlet-instance
  (define (create-new-instance! instance-table invoke-id)
    (let ([inst (make-servlet-instance invoke-id (make-hash-table) 0
                                       (make-semaphore 0))])
      (hash-table-put! instance-table invoke-id inst)
      inst))

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

  ;; continuation-url?: url -> (union (list number number) #f)
  ;; determine if this url encodes a continuation and extract the instance id and
  ;; continuation id.
  (define (continuation-url? a-url)
    (let ([str (url->param a-url)])
      (and str
           (let ([param-match (cdr (match-url-params str))])
             (list (string->symbol (car param-match))
                   (string->number (cadr param-match)))))))

  ;; url->param: url -> (union string #f)
  (define (url->param a-url)
    (let ([l (filter path/param? (url-path a-url))])
      (and (not (null? l))
           (path/param-param (car l)))))

  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (url->string
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
      in-url)))

  ;; replace-path: (url-path -> url-path) url -> url
  ;; make a new url by replacing the path part of a url with a function
  ;; of the url's old path
  (define (replace-path proc in-url)
    (let ([new-path (proc (url-path in-url))])
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       new-path
       (url-query in-url)
       (url-fragment in-url))))

  ;; **************************************************

  )
