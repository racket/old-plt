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
      
      (define (matches? open close)
        (equal? (hash-table-get open-matches-table open (lambda () #f))
                close))
      
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
               ((>= start (+ node-start (node-token-length tree)))
                (set! tree (insert-next! tree (make-node length (cons type length) 0 #f #f))))
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
      
      (define/public (remove-token start len)
        (printf "remove ~a ~a~n" start len)
        (set! tree (search! tree start))
        (cond
          ((= start 0)
           (set! tree (search-min! tree null))
           (cond
             ((= (node-token-length tree) len)
              (set! tree (node-right tree)))
             (else
              (let ((data (node-token-data tree)))
                (when (or (is-open? (car data)) (is-close? (car data)))
                  (set-node-token-data! tree (cons #f (cdr data))))
                (set-node-token-length! tree (- (node-token-length tree) len))))))
          ((and (> start 0) (= start (node-left-subtree-length tree)))
           (let ((len (- (node-token-length tree) len)))
             (set! tree (search! (remove-root! tree) (sub1 start)))
             (set-node-token-length! tree (+ (node-token-length tree) len))))
          (else
           (set-node-token-length! tree (- (node-token-length tree) len)))))
      
      (define/public (match pos)
        (set! tree (search! tree pos))
        (cond
          (tree
           (cond
             ((and (is-open? (car (node-token-data tree)))
                   (= (node-left-subtree-length tree) pos))
              (printf "depth:~a size: ~a~n" (max-depth (node-right tree))
                      (size (node-right tree) 0))
              (let/ec ret
                (match-forward (node-right tree)
                               (+ (node-left-subtree-length tree) (node-token-length tree))
                               (list (car (node-token-data tree)))
                               ret)
                #f))
             (else #f)))
          (else #f)))
      
        
      (define/public (match-forward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (car (node-token-data node)))
                  (left-stack (match-forward (node-left node) top-offset stack escape))
                  (new-stack
                   (cond
                     ((is-open? type) (cons type left-stack))
                     ((and (is-close? type) (matches? (car left-stack) type))
                      (cdr left-stack))
                     ((is-close? type) (escape #f))
                     (else left-stack)))
                  (start (+ top-offset (node-left-subtree-length node))))
             (cond
               ((null? new-stack)
                (let ((loc (sub1 (+ start (cdr (node-token-data node))))))
                  (set! tree (search! tree loc))
                  (escape loc)))
               (else
                (match-forward (node-right node) (+ start (node-token-length node)) new-stack escape)))))))
      
      
      (define/public (match-reverse pos)
        (void))
      
      (define/public (print)
        (for-each (lambda (x) (display (cons (vector-ref x 0) (car (vector-ref x 2)))))
                  (to-list tree))
        (newline))
      
      (super-instantiate ())
      )))