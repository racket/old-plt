(module finddoc mzscheme
  (provide finddoc)
  
  (define ht (make-hash-table))
  
  (define (finddoc manual index-key label)
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
            (format "<A href=\"file:~a\">~a</A>"
                    (build-path docdir (cadr m))
                    label)))))))
