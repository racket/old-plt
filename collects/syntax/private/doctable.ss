
(module doctable mzscheme

  (define ht (make-hash-table))

  (define (register-documentation src-stx label v)
    (let ([mod (syntax-source-module src-stx)])
      (let ([mht (hash-table-get ht mod
				 (lambda ()
				   (let ([mht (make-hash-table)])
				     (hash-table-put! ht mod mht)
				     mht)))])
	(hash-table-put! mht label v))))

  (define (lookup-documentation mod label)
    (let ([mht (hash-table-get ht mod (lambda () #f))])
      (and mht
	   (hash-table-get mht label (lambda () #f)))))

  (provide register-documentation
	   lookup-documentation))
