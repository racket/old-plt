(module paren-tree mzscheme
  (require (lib "class.ss")
           "token-tree.ss")
  
  (provide paren-tree%)
  
  
  (define paren-tree%
    (class object%

      (init matches)
      
      (define open-matches-table (make-hash-table))
      (for-each (lambda (x)
                  (hash-table-put! open-matches-table (car x) (cadr x)))
                matches)

      (define close-matches-table (make-hash-table))
      (for-each (lambda (x)
                  (hash-table-put! close-matches-table (cadr x) (car x)))
                matches)
      
      (define tree #f)
      
      (define (is-open? x)
        (hash-table-get open-matches-table x (lambda () #f)))
      
      (define (is-close? x)
        (hash-table-get close-matches-table x (lambda () #f)))
      
      (define/public (add-token type start length)
        (cond
          ((not tree)
           (set! tree (make-node length (cons type length) 0 #f #f)))
          ((or (is-open? type) (is-close? type))
           (set! tree (search! tree start))
           (let ((node-start (node-left-subtree-length tree)))
             (cond
               ((= node-start start)
                (set! tree (insert-prev! tree (make-node length (cons type length) 0 #f #f))))
               (else
                (let ((extra-length (- (node-token-length tree)
                                       (cdr (node-token-data tree)))))
                  (set-node-token-length! tree (cdr (node-token-data tree)))
                  (set! tree (insert-next! tree (make-node (+ length extra-length)
                                                           (cons type length)
                                                           0
                                                           #f
                                                           #f))))))))
          (else
           (set! tree (search! tree (if (> 0 start) (sub1 start) start)))
           (set-node-token-length! tree (+ (node-token-length tree) length)))))
      
      (define/public (remove-token type start end)
        void)
      
      (define/public (match-forward pos)
        void)
      
      (define/public (match-reverse pos)
        void)
      
      (define/public (print)
        (let ((l (to-list tree)))
          (for-each (lambda (x)
                      (display (symbol->string (car (vector-ref x 2))))
                      l))))
      
      )))