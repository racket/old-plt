
(let ([ht (make-hash-table)])
  (lambda (manual index-key label)
    (with-handlers ([void (lambda (x) "???")])
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
	    (format "<A href=\"~a\">~a</A>"
		    (build-path docdir (cadr m))
		    label)))))))
