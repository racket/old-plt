(define all-emails (map cadddr (call-with-input-file "raw-kajitani" read)))

(define ht (make-hash-table))

(for-each
 (lambda (x)
   (when x
     (hash-table-put! ht (string->symbol x) #t)))
 all-emails)

(define unique-emails (hash-table-map ht (lambda (x y) (symbol->string x))))

(call-with-output-file "EMAIL"
  (lambda (p)
    (write (car unique-emails) p)
    (for-each (lambda (x) (display ", " p) (write x p)) (cdr unique-emails)))
  'truncate)
