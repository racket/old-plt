(module red-sem-macro-helpers mzscheme
  
  (provide extract-names)
  
  (define (extract-names stx)
    (let ([dup-names
           (let loop ([sexp (syntax-object->datum stx)]
                      [names null])
             (cond
               [(and (pair? sexp)
                     (eq? 'name (car sexp))
                     (pair? (cdr sexp))
                     (symbol? (cadr sexp))
                     (pair? (cddr sexp))
                     (null? (cdddr sexp)))
                (loop (caddr sexp) (cons (cadr sexp) names))]
               [(list? sexp)
                (let i-loop ([sexp sexp]
                             [names names])
                  (cond
                    [(null? sexp) names]
                    [else (i-loop (cdr sexp) (loop (car sexp) names))]))]
               [else names]))]
          [ht (make-hash-table)])
      (for-each (lambda (name) (hash-table-put! ht name #f)) dup-names)
      (hash-table-map ht (lambda (x y) x)))))