
(module doctable mzscheme

  (define ht (make-hash-table))

  (define (register-documentation src-stx rows)
    (let ([mod (syntax-source-module src-stx)])
      (hash-table-put! ht mod rows)))

  (define (lookup-documentation mod)
    (hash-table-get ht mod (lambda () #f)))

  (provide register-documentation
	   lookup-documentation))
