(module token-tree mzscheme
  
  (provide search! search-min! search-max! insert-after! insert-before! 
           split insert-next! insert-prev! remove-root!
           to-list size max-depth
           (struct node (token-length token-data left-subtree-length left right)))
  
  (define-struct node (token-length token-data left-subtree-length left right))
  
  (define (search! node key-position)
    (cond
      (node
       (internal-search! node key-position 0 null))
      (else #f)))
  
  ;; key-position is the position in the buffer we are looking for
  ;; offset is the offset for the whole subtree of node in the buffer.
  ;; path is the path back to the root
  (define (internal-search! node key-position offset path)
    (cond
      ((not node)
       (end-search! path))
      (else
       (let* ((node-start (+ offset (node-left-subtree-length node)))
              (node-end (+ node-start (node-token-length node))))
         (cond
           ((< key-position node-start)
            (internal-search! (node-left node) key-position offset (cons node path)))
           ((>= key-position node-end)
            (internal-search! (node-right node) key-position node-end (cons node path)))
           (else
            (bottom-up-splay! node path)))))))
      
  (define (end-search! path)
    (cond
      ((null? path) #f)
      (else (bottom-up-splay! (car path) (cdr path)))))
  
  (define (search-max! node path)
    (cond
      ((not node)
       (end-search! path))
      (else
       (search-max! (node-right node) (cons node path)))))
       
  (define (search-min! node path)
    (cond
      ((not node)
       (end-search! path))
      (else
       (search-min! (node-left node) (cons node path)))))
  
  (define (invalidate-after! node pos)
    (let ((n (search! node pos)))
      (when n
        (set-node-right! n #f))
      n))
  
  (define (invalidate-before! node pos)
    (let ((n (search! node pos)))
      (when n
        (set-node-left! n #f)
        (set-node-left-subtree-length! n 0))
      n))
  
  (define (split node pos)
    (let ((n (search! node pos)))
      (cond
        (n (values (node-left-subtree-length n)
                   (+ (node-left-subtree-length n) (node-token-length n))
                   (node-left n)
                   (node-right n)))
        (else (values 0 0 #f #f)))))

  (define (remove-root! node)
    (let ((new-node (search-max! (node-left node) null)))
      (set-node-right! new-node (node-right node))
      new-node))
  
  (define (insert-before! node new-node)
    (let ((n (search-min! node null)))
      (cond
        (n
         (set-node-left! n new-node)
         (search-min! n null))
        (else new-node))))
  
  (define (insert-after! node new-node)
    (let ((n (search-max! node null)))
      (cond
        (n
         (set-node-right! n new-node)
         (search-max! n null))
        (else new-node))))
  
  (define (insert-prev! node new-node)
    (set-node-left! node (insert-after! (node-left node) new-node))
    (set-node-left-subtree-length! node (+ (node-token-length new-node) 
                                           (node-left-subtree-length new-node)))
    node)
  
  (define (insert-next! node new-node)
    (set-node-right! node (insert-before! (node-right node) new-node))
    node)
  
  
  (define (update-subtree-length-left-rotate! self parent)
    (set-node-left-subtree-length! parent 
                                   (- (node-left-subtree-length parent)
                                      (node-left-subtree-length self)
                                      (node-token-length self))))

  (define (update-subtree-length-right-rotate! self parent)
    (set-node-left-subtree-length! self
                                   (+ (node-left-subtree-length parent)
                                      (node-left-subtree-length self)
                                      (node-token-length parent))))
  
  (define (bottom-up-splay! self path)
    (cond
      ((null? path) self)               ;; node is root already
      ((null? (cdr path))               ;; node's parent is root
       (let ((parent (car path)))
         (cond
           ((eq? self (node-left parent))
            (set-node-left! parent (node-right self))
            (set-node-right! self parent)
            (update-subtree-length-left-rotate! self parent))
           (else
            (set-node-right! parent (node-left self))
            (set-node-left! self parent)
            (update-subtree-length-right-rotate! self parent))))
       self)
      (else
       (let ((grand (cadr path))
             (parent (car path)))
         (cond
           ((eq? self (node-left parent))
            (cond
              ((eq? parent (node-left grand))
               (set-node-left! grand (node-right parent))
               (set-node-right! parent grand)
               (set-node-left! parent (node-right self))
               (set-node-right! self parent)
               (update-subtree-length-left-rotate! parent grand)
               (update-subtree-length-left-rotate! self parent))
              (else
               (set-node-right! grand (node-left self))
               (set-node-left! self grand)
               (set-node-left! parent (node-right self))
               (set-node-right! self parent)
               (update-subtree-length-left-rotate! self parent)
               (update-subtree-length-right-rotate! self grand))))
           (else
            (cond
              ((eq? parent (node-right grand))
               (set-node-right! grand (node-left parent))
               (set-node-left! parent grand)
               (set-node-right! parent (node-left self))
               (set-node-left! self parent)
               (update-subtree-length-right-rotate! parent grand)
               (update-subtree-length-right-rotate! self parent))
              (else
               (set-node-left! grand (node-right self))
               (set-node-right! self grand)
               (set-node-right! parent (node-left self))
               (set-node-left! self parent)
               (set-node-left-subtree-length! grand
                                              (- (node-left-subtree-length grand)
                                                 (node-left-subtree-length parent)
                                                 (node-token-length parent)
                                                 (node-left-subtree-length self)
                                                 (node-token-length self)))
               (update-subtree-length-right-rotate! self parent)))))
         (if (not (null? (cddr path)))
             (if (eq? grand (node-left (caddr path)))
                 (set-node-left! (caddr path) self)
                 (set-node-right! (caddr path) self)))
         (bottom-up-splay! self (cddr path))))))

  (define (size node acc)
    (cond
      ((not node) acc)
      (else
       (let ((left-size (size (node-left node) acc)))
         (size (node-right node) (add1 left-size))))))
  
  (define (max-depth node)
    (cond
      ((not node) 0)
      (else
       (add1 (max (max-depth (node-left node))
                  (max-depth (node-right node)))))))
      
  (define (to-list node)
    (cond
      (node
       (append (to-list (node-left node))
               (list (vector (node-token-length node) (node-left-subtree-length node) (node-token-data node)))
               (to-list (node-right node))))
      (else null)))
  )