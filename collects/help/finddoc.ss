
(let ([ht (make-hash-table)])
  (lambda (manual index-key label)
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
	    (format "<A href=\"~a\">~a</A>"
		    (fs-path->file://path (build-path docdir (cadr m)))
		    label)))))))
