(module finddoc mzscheme
  (provide finddoc
           findreldoc)
  
  ;; Creates a "file:" link into the indicated manual.
  ;; The link doesn't go to a particular anchor,
  ;; because "file:" does not support that.
  (define (finddoc manual index-key label)
    (let ([m (lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"file:~a\">~a</A>"
                  (build-path (car m) (caddr m))
                    label))))
  
  ;; Given a Unix-style relative path to reach the "doc"
  ;; collection, creates a link that can go to a
  ;; particular anchor.
  (define (findreldoc todocs manual index-key label)
    (let ([m (lookup manual index-key label)])
      (if (string? m)
          m
          (format "<A href=\"~a/~a/~a#~a\">~a</A>"
                  todocs
                  manual
                  (caddr m)
                  (cadddr m)
                  label))))
  
  (define ht (make-hash-table))
  
  ;; returns either a string (failure) or
  ;; (list docdir index-key filename anchor title)
  (define (lookup manual index-key label)
    (with-handlers ([(lambda (x) #t) 
                     (lambda (x) 
                       (format 
                        "<font color=\"red\">[internal error:finddoc didn't find link; manual: ~s index-key: ~s label: ~s]</font>"
                        manual index-key label))])
      (let ([key (string->symbol manual)]
            [docdir (build-path (collection-path "doc") manual)])
        (let ([l (hash-table-get
                  ht
                  key
                  (lambda ()
                    (let ([f (build-path docdir "hdindex")])
                      (let ([l (with-input-from-file f read)])
                        (hash-table-put! ht key l)
                        l))))])
          (let ([m (assoc index-key l)])
            (if m 
                (cons docdir m)
                (raise 'not-there))))))))
