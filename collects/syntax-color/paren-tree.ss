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
      
      (define (is-open? x)
        (hash-table-get open-matches-table x (lambda () #f)))
      
      (define (is-close? x)
        (hash-table-get close-matches-table x (lambda () #f)))
      
      (define (matches? open close)
        (equal? (hash-table-get open-matches-table open (lambda () #f))
                close))

      (define tree #f)
      (define invalid-tree #f)
      
      (define (split tree pos)
        (cond
          (tree
           (let ((t (search! tree pos)))
             (cond
               ((= pos (node-left-subtree-length t))
                (values (node-left t)
                        (begin
                          (set-node-left! t #f)
                          (set-node-left-subtree-length! t 0)
                          t)))
               (else
                (values (make-node (- pos (node-left-subtree-length t))
                                   (node-token-data t)
                                   (node-left-subtree-length t)
                                   (node-left t)
                                   #f)
                        (make-node (- (+ (node-token-length t) (node-left-subtree-length t))
                                      pos)
                                   (cons #f #f)
                                   0
                                   #f
                                   (node-right t)))))))
          (else (values #f #f))))

      
      (define/public (split-tree pos)
        (let-values (((l r) (split tree pos)))
          (set! tree l)
          (set! invalid-tree r)))
        
      (define/public (merge-tree num-to-keep)
        (set! invalid-tree (search-max! invalid-tree null))
        (let-values (((bad good) (split invalid-tree (- (+ (node-token-length invalid-tree)
                                                           (node-left-subtree-length invalid-tree))
                                                        num-to-keep))))
          (when (not (or (is-open? (car (node-token-data good)))
                         (is-close? (car (node-token-data good)))))
            (add-token #f (node-token-length good))
            (set! good (node-right good)))
          (set! tree (insert-after! tree good))
          (set! invalid-tree #f)))
      
      (define/public (add-token type length)
        (cond
          ((or (not tree) (is-open? type) (is-close? type))
           (set! tree (insert-after! tree (make-node length (cons type length) 0 #f #f))))
          (else
           (set! tree (search-max! tree null))
           (set-node-token-length! tree (+ (node-token-length tree) length)))))
      
      (define/public (match-forward pos)
        (set! tree (search! tree pos))
        (cond
          ((and tree
                (node-token-data tree)
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
      
      (define/public (print)
        (for-each (lambda (x) (display (cons (vector-ref x 0) (car (vector-ref x 2)))))
                  (to-list tree))
        (newline))
      
      (super-instantiate ())
      )))