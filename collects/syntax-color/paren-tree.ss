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

      (define tree (new token-tree%))
      (define invalid-tree (new token-tree%))
      
      (define (split tree pos)
        (send tree search! pos)
        (let ((token-start (send tree get-root-start-position)))
          (cond
            ((send tree is-empty?)
             (values (new token-tree%) (new token-tree%)))
            ((= pos token-start)
             (send tree split-before))
            (else
             (let-values (((first next) (send tree split-after)))
               (let ((first-end (send first get-root-end-position)))
                 (send first add-to-root-length (- pos first-end))
                 (insert-first! next (new token-tree%
                                          (length (- first-end pos))
                                          (data (cons #f 0))))
                 (values first next)))))))
      
      (define/public (split-tree pos)
        (let-values (((l r) (split tree pos)))
          (set! tree l)
          (set! invalid-tree r)))
      
      (define/public (merge-tree num-to-keep)
        (send invalid-tree search-max!)
        (let*-values (((bad good) (split invalid-tree (- (send invalid-tree get-root-end-position)
                                                         num-to-keep)))
                      ((data) (send good get-root-data)))
          (when (and data
                     (not (or (is-open? (car data))
                              (is-close? (car data)))))
            (add-token #f (send good get-root-length))
            (send good remove-root!))
          (insert-last! tree good)))
      
      (define/public (add-token type length)
        (cond
          ((or (send tree is-empty?) (is-open? type) (is-close? type))
           (insert-last! tree (new token-tree% (length length) (data (cons type length)))))
          (else
           (send tree search-max!)
           (send tree add-to-root-length length))))
      
      (define/public (match-forward pos)
        (send tree search! pos)
        (cond
          ((and (not (send tree is-empty?))
                (is-open? (car (send tree get-root-data)))
                (= (send tree get-root-start-position) pos))
           (let ((end
                  (let/ec ret
                    (do-match-forward (node-right (send tree get-root))
                                      (send tree get-root-end-position)
                                      (list (car (send tree get-root-data)))
                                      ret)
                    #f)))
             (cond
               (end
                (values pos end #f))
               (else
                (send tree search-max!)
                (let ((end (send tree get-root-end-position)))
                  (send tree search! pos)
                  (values pos (+ pos (cdr (send tree get-root-data))) end))))))
          (else
           (values #f #f #f))))
      
      (define/public (match-backward pos)
        (send tree search! (if (> pos 0) (sub1 pos) pos))
        (cond
          ((and (not (send tree is-empty?))
                (is-close? (car (send tree get-root-data)))
                (= (+ (cdr (send tree get-root-data))
                      (send tree get-root-start-position))
                   pos))
           (let ((end
                  (let/ec ret
                    (do-match-backward (node-left (send tree get-root))
                                       0
                                       (list (car (send tree get-root-data)))
                                       ret)
                    #f)))
             (cond
               (end
                (values end pos #f))
               (else
                (send tree search! pos)
                (values (- pos (cdr (send tree get-root-data))) pos #t)))))
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
                  (escape loc)))
               (else
                (do-match-forward (node-right node) (+ start (node-token-length node)) new-stack escape)))))))
      
      (define (do-match-backward node top-offset stack escape)
        (cond
          ((not node) stack)
          (else
           (let* ((type (car (node-token-data node)))
                  (right-stack (do-match-backward (node-right node)
                                                  (+ top-offset (node-left-subtree-length node)
                                                     (node-token-length node))
                                                  stack escape))
                  (new-stack
                   (cond
                     ((is-close? type) (cons type right-stack))
                     ((and (is-open? type) (matches? type (car right-stack)))
                      (cdr right-stack))
                     ((is-open? type) (escape #f))
                     (else right-stack))))
             (cond
               ((null? new-stack)
                (escape (+ top-offset (node-left-subtree-length node))))
               (else
                (do-match-backward (node-left node) top-offset new-stack escape)))))))
      
      (define/public (print)
        (for-each (lambda (x) (display (cons (vector-ref x 0) (car (vector-ref x 2)))))
                  (send tree to-list))
        (newline))
      
      (super-instantiate ())
      )))
