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
                (let ((old-length (node-token-length tree))
                      (new-length (- start node-start)))
                  (set-node-token-length! tree new-length)
                  (set! tree (insert-next! tree (make-node (+ length (- old-length new-length))
                                                           (cons type length)
                                                           0
                                                           #f
                                                           #f))))))))
          (else
           (set! tree (search! tree (if (> start 0) (sub1 start) start)))
           (set-node-token-length! tree (+ (node-token-length tree) length)))))
      
      (define/public (remove-token start len)
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
      
      (define/public (match-forward pos)
        (set! tree (search! tree pos))
        (cond
          ((and tree
                (is-open? (car (node-token-data tree)))
                (= (node-left-subtree-length tree) pos))
           (let ((end
                  (let/ec ret
                    (do-match-forward (node-right tree)
                                      (+ (node-left-subtree-length tree) (node-token-length tree))
                                      (list (car (node-token-data tree)))
                                      ret)
                    #f)))
             (cond
               (end
                (values pos end #f))
               (else
                (set! tree (search! tree pos))
                (values pos (+ pos (cdr (node-token-data tree))) #t)))))
          (else
           (values #f #f #f))))
        
      (define (do-match-forward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (car (node-token-data node)))
                  (left-stack (do-match-forward (node-left node) top-offset stack escape))
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
                (let ((loc (+ start (cdr (node-token-data node)))))
                  (set! tree (search! tree loc))
                  (escape loc)))
               (else
                (do-match-forward (node-right node) (+ start (node-token-length node)) new-stack escape)))))))
      
      
      (define/public (match-backward pos)
        (values #f #f #f))
      
      (define/public (print)
        (for-each (lambda (x) (display (cons (vector-ref x 0) (car (vector-ref x 2)))))
                  (to-list tree))
        (newline))
      
      (super-instantiate ())
      )))