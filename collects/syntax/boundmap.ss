
(module boundmap mzscheme
  (require (lib "contract.ss")
	   (lib "etc.ss"))

  (define-struct bound-identifier-mapping (ht))

  (define mk-bound-identifier-mapping
    (let ([make-bound-identifier-mapping
	   (lambda ()
	     (make-bound-identifier-mapping
	      (make-hash-table)))])
      make-bound-identifier-mapping))

  (define bound-identifier-mapping-get
    (opt-lambda (bi id [fail (lambda () 
			       (error 'bound-identifier-mapping-get
				      "no mapping for ~e"
				      id))])
      (or (ormap (lambda (i)
		   (and (bound-identifier=? (car i) id)
			(cdr i)))
		 (hash-table-get (bound-identifier-mapping-ht bi)
				 (syntax-e id) 
				 fail))
	  (fail))))

  (define bound-identifier-mapping-put!
    (lambda (bi id v)
      (let ([l (hash-table-get
		(bound-identifier-mapping-ht bi)
		(syntax-e id) 
		(lambda () null))])
	(hash-table-put!
	 (bound-identifier-mapping-ht bi)
	 (syntax-e id) 
	 (let loop ([l l])
	   (cond
	    [(null? l) (list (cons id v))]
	    [(bound-identifier=? (caar l) id)
	     (cons (cons id v) l)]
	    [else (cons (car l) (loop (cdr l)))]))))))
  
  (provide (rename mk-bound-identifier-mapping
		   make-bound-identifier-mapping)
	   bound-identifier-mapping?)
  (provide/contract
   [bound-identifier-mapping-get ((bound-identifier-mapping?
				   identifier?)
				  ((-> any))
				  . opt-> .
				  any?)] ;; !!!!!!!! FIXME : result should be `any', not `any?'
   [bound-identifier-mapping-put! (bound-identifier-mapping?
				   identifier?
				   any?
				   . -> .
				   void?)]))
