(module utils mzscheme
  (require (lib "url.ss" "net"))
  (provide url->servlet-path
           make-session-url)

  ;; make-session-url: url (listof string) -> url
  ;; produce a new url for this session:
  ;;   Minimal path to the servlet.
  ;;   No query.
  ;;   No fragment.
  (define (make-session-url uri new-path)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     new-path
     '()
     #f
     ))

  ;; build-root-path: -> path
  ;; build the root path for whatever this OS is
  (define (build-root-path)
    (let loop ([prev (simplify-path (build-path 'same))]
               [next (simplify-path (build-path 'up))])
      (if (equal? prev next)
          prev
          (loop next
                (simplify-path (build-path next 'up))))))

  (define the-root-path (build-root-path))

  ;; simplify-url-path: url -> (listof string)
  ;; take the dots out of the url-path
  ;; Note: we simplify the url path relative to a hypothetical root,
  ;;       so that a malicious url can't cause the server to chase ".."
  ;;       up beyond the legitimate servlet root.
  (define (simplify-url-path uri)
    (path->list
     (simplify-path
      (apply build-path
             (cons the-root-path
                   (map
                    (lambda (str)
                      (if (string=? str "")
                          'same
                          str))
                    (map
                     (lambda (path-elt)
                       (if (path/param? path-elt)
                           (path/param-path path-elt)
                           path-elt))
                     (url-path uri))))))))

  ;; path->list pth
  ;; convert an absolute path to a list of strings
  (define (path->list pth)
    (reverse
     (let path->list ([pth pth])
       (let-values ([(base name must-be-dir?) (split-path pth)])
         (if base
             (cons (path->string name) (path->list base))
             '())))))


  ;; url->servlet-path: path url -> (union path #f)
  ;; given a servlet directory and url, find a servlet
  (define (url->servlet-path servlet-dir uri)
    (let loop ([base-path servlet-dir]
               [servlet-path '()]
               [path-list (simplify-url-path uri)])
      (if
       (null? path-list)
       (values #f #f #f)
       (let* ([next-path-segment (car path-list)]
              [new-base (build-path base-path next-path-segment)])
         (cond
           [(file-exists? new-base)
            (values new-base
                    (reverse (cons next-path-segment servlet-path))
                    (cdr path-list))]
           [else (loop new-base
                       (cons next-path-segment servlet-path)
                       (cdr path-list))])))))
  )
