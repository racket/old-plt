(module finddoc mzscheme
  (provide finddoc-page)
  
  ; finddoc-page : string string -> string
  ; returns just the page html, suitable for use by PLT Web server
  (define (finddoc-page manual index-key)
    (let ([m (lookup manual index-key "dummy")])
      (if (string? m)
          m
          (caddr m))))

  (define ht (make-hash-table))
  
  ;; returns either a string (failure) or
  ;; (list docdir index-key filename anchor title)
  (define (lookup manual index-key label)
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
	      (raise 'not-there)))))))


