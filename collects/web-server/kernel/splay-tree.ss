(module splay-tree mzscheme
  (require (lib "unitsig.ss")
           (lib "list.ss"))
  
  (provide comparable^ splay-tree^ splay-tree@)
  
  (define-signature comparable^ (same? less?))
  (define-signature splay-tree^
    (tree-key access insert delete join split smallest largest
              fold-ascending fold-descending tree-for-each!))
  
  (define splay-tree@
    (unit/sig splay-tree^
      (import comparable^)
      
      ;; ********************************************************************************
      ;; The algorithm works by finding the node x in the binary tree and then performing
      ;; splay operations until the final resulting tree has x located at the root.
      ;; 
      ;; Typically, a splay operation involves a node, x, its parent node and its 
      ;; grandparent node. The parent/grandparent relation is considered only along the
      ;; path from the root to x and so x has exactly one parent and one grandparent.
      ;; These typical splay operations are called zigzig, zigzag, zagzig and zagag.
      ;;
      ;; If after several splays, x is exactly one away from the root. Then a special
      ;; splay operation involving only x and its parent is performed. These special
      ;; operations are called zig and zag.
      ;;
      ;; Because splaying starts at x and moves upward in the tree, the algorithm must
      ;; keep track of some state along the way to x. To accomplish this, splay is
      ;; implemented to keep track of 1, 2, or 3 nodes at a time.
      ;; 
      ;; In short, this is just a complicated way of "cdr-ing" down the tree and then
      ;; "cons-ing" back up.
      
      ;; NOTES
      ;;
      ;; 1. I code walked this with Richard Cobbe the conclusions were
      ;;  a) What I have is too complicated.
      ;;  b) An alternative is to traverse the tree once to determine whether it is an
      ;;     odd lenght path and then splay will take nodes two at a time from either
      ;;     the root or the first child of the root. This solves the "return twice"
      ;;     problem (get's rid of the call/ec confusion). Drawback is that you must
      ;;     traverse the tree twice.
      ;;  c) In light of b, the extra traversal could be avoided by writing splay in CPS
      ;;     but use two continuations. One for an odd length path and one for an even.
      ;;     Upon finding x, then call the appropriate continuation.
      ;; 2. Another alternative. First find the path and accumulate it in a list in order
      ;;    from x to the root. Then splay down the list to create the new tree. I
      ;;    attempted this, but the problem I ran into was that as you build the new tree,
      ;;    the nodes in the list become out of date. This would be avoided if you built
      ;;    the tree by mutating the nodes.
      
      (define-struct tree (key left right) (make-inspector))
      
      ;; ********************************************************************************
      ;; a tree-xform is either
      ;; tree tree -> tree
      ;; tree tree tree -> tree
      
      ;; rotate-right the edge joining parent to x
      (define (zig parent-tree new-x-tree)
        (make-tree
         (tree-key new-x-tree)
         (tree-left new-x-tree)
         (make-tree
          (tree-key parent-tree)
          (tree-right new-x-tree)
          (tree-right parent-tree))))
      
      ;; rotate-left the edge joining parent to x
      (define (zag parent-tree new-x-tree)
        (make-tree
         (tree-key new-x-tree)
         (make-tree
          (tree-key parent-tree)
          (tree-left parent-tree)
          (tree-left new-x-tree))
         (tree-right new-x-tree)))      
      
      ;; rotate-right the edge joining parent to x
      ;; then rotate-left the edge now joining grandparent to x
      (define (zigzag grandparent-tree parent-tree new-x-tree)
        (make-tree
         (tree-key new-x-tree)
         (make-tree
          (tree-key grandparent-tree)
          (tree-left grandparent-tree)
          (tree-left new-x-tree))
         (make-tree
          (tree-key parent-tree)
          (tree-right new-x-tree)
          (tree-right parent-tree))))
      
      ;; rotate-left the edge joining parent to x
      ;; then rotate-right the edge now joining grandparent to x
      (define (zagzig grandparent-tree parent-tree new-x-tree)
        (make-tree
         (tree-key new-x-tree)
         (make-tree
          (tree-key parent-tree)
          (tree-left parent-tree)
          (tree-left new-x-tree))
         (make-tree
          (tree-key grandparent-tree)
          (tree-right new-x-tree)
          (tree-right grandparent-tree))))
      
      ;; rotate-right the edge joining grandparent to parent
      ;; then rotate-right the edge joining parent to x
      (define (zigzig grandparent-tree parent-tree x-tree)
        (make-tree
         (tree-key x-tree)
         (tree-left x-tree)
         (make-tree
          (tree-key parent-tree)
          (tree-right x-tree)
          (make-tree
           (tree-key grandparent-tree)
           (tree-right parent-tree)
           (tree-right grandparent-tree)))))
      
      ;; rotate-left the edge joining grandparent to parent
      ;; then rotate-right the edge joining parent to x
      (define (zagzag grandparent-tree parent-tree x-tree)
        (make-tree
         (tree-key x-tree)
         (make-tree
          (tree-key parent-tree)
          (make-tree
           (tree-key grandparent-tree)
           (tree-left grandparent-tree)
           (tree-left parent-tree))
          (tree-left x-tree))
         (tree-right x-tree)))
      
      ;; splay is either ...     
      (define splay
        (case-lambda
          
          ;; (tree -> tree-xform tree) tree -> tree
          [(next-tree x-tree)
           (if (null? x-tree) '()
               (let-values ([(z*g next-x) (next-tree x-tree)])
                 (splay next-tree z*g x-tree next-x)))]
          
          ;; (tree-xform tree -> tree-xform tree) tree tree -> tree
          [(next-tree z*g p x-tree)
           (if (null? x-tree) p
               (let-values ([(z*gz*g next-x)
                             (next-tree z*g x-tree)])
                 (let/ec k
                   (z*g p (splay next-tree k z*gz*g p x-tree next-x)))))]
          
          ;; (tree-xform tree -> tree-xform tree) (tree -> tree) tree-xform tree tree tree -> tree
          [(next-tree k z*gz*g g p x)
           (if (null? x) p
               (let-values ([(next-z*gz*g next-x) (next-tree z*gz*g x)])
                 (let/ec k2
                   (k (z*gz*g g p (splay next-tree k2 next-z*gz*g p x next-x))))))]))
      
      ;; make-next-tree: (-> alpha) (tree -> alpha) (tree-> alpha) -> alpha -> find-next-proc
      ;; where find-next-proc is either ...
      ;; The find-next procedure produces the next-tree in the path to x and the next tree
      ;; transformer.
      (define (make-next-tree find-next)
        (case-lambda
          
          ;; tree -> tree-xform tree
          ;; given the current tree produce the next-tree and the next transformer
          [(x-tree)
           (find-next
            x-tree
            
            ;; none
            (lambda () (values #f '()))
            
            ;; zig
            (lambda (next-x)  (values zig next-x))
            
            ;; zag
            (lambda (next-x) (values zag next-x)))]
          
          ;; tree-xform tree -> tree-xform tree
          ;; given the current tree and the current transformer produce the next ones
          [(z*gz*g x-tree)
           (find-next
            x-tree
            
            ;; none
            (lambda () (values #f '()))
            
            ;; zig
            (lambda (next-x)
              (cond
                [(or (eq? z*gz*g zig) (eq? z*gz*g zigzig) (eq? z*gz*g zigzag))
                 (values zigzig next-x)]
                [(or (eq? z*gz*g zag) (eq? z*gz*g zagzig) (eq? z*gz*g zagzag))
                 (values zigzag next-x)]))
            
            (lambda (next-x)
              (cond
                [(or (eq? z*gz*g zig) (eq? z*gz*g zigzig) (eq? z*gz*g zigzag))
                 (values zagzig next-x)]
                
                [(or (eq? z*gz*g zag) (eq? z*gz*g zagzig) (eq? z*gz*g zagzag))
                 (values zagzag next-x)])))]))
      
      
      ;; ********************************************************************************
      ;; ********************************************************************************
      ;; EXPORTS
      
      ;; access: key tree -> (union tree '())
      ;; find the node with the given key in the tree
      (define (access key in-tree)
        (splay (make-access-next key) in-tree))
      
      (define (make-access-next key)
        (make-next-tree
         (lambda (a-tree null-proc zig-proc zag-proc)
           (let ([key-prime (tree-key a-tree)])
             (cond
               [(same? key key-prime) (null-proc)]
               [(less? key key-prime)
                (let ([left (tree-left a-tree)])
                  (if (null? left) (null-proc)
                      (zig-proc left)))]
               [else
                (let ([right (tree-right a-tree)])
                  (if (null? right) (null-proc)
                      (zag-proc right)))])))))
      
      ;; *********************************************************************
      ;; join: tree tree -> tree
      ;; Combine trees t1 and t2 into a single tree containing all items from
      ;; both trees and return the resulting tree. This operation assumes that
      ;; all items in t1 are less than all those in t2.
      (define (join t1 t2)
        (if (null? t1) t2
            (let ([new-t1 (largest t1)])
              (make-tree
               (tree-key new-t1)
               (tree-left new-t1)
               t2))))
      
      ;; ********************************************************************
      ;; smallest: tree -> tree
      ;; return a tree with the same elements as the given tree, and with the 
      ;; smallest at the root
      (define (smallest a-tree)
        (splay smallest-next a-tree))
      
      (define smallest-next
        (make-next-tree
         (lambda (a-tree null-proc zig-proc zag-proc)
           (let ([next-tree (tree-left a-tree)])
             (if (null? next-tree)
                 (null-proc)
                 (zig-proc next-tree))))))
      
      ;; ********************************************************************
      ;; largest: tree -> tree
      ;; return a tree with the same elements as the given tree, and with the
      ;; largest at the root
      (define (largest a-tree)
        (splay largest-next a-tree))
      
      (define largest-next
        (make-next-tree
         (lambda (a-tree null-proc zig-proc zag-proc)
           (let ([next-tree (tree-right a-tree)])
             (if (null? next-tree)
                 (null-proc)
                 (zag-proc next-tree))))))
      
      ;; ****************************************************************************
      ;; split: key tree -> (union tree '()) (union tree '())
      ;; Construct and return two trees t1 and t2, where t1 contains all items
      ;; in t less than or equal to i, and t2 contains all items in t greater than i.
      (define (split key a-tree)
        (let ([new-tree (access key a-tree)])
          (cond
            [(null? new-tree) (values '() '())]
            [(less? key (tree-key new-tree))
             (values (tree-left new-tree)
                     (make-tree
                      (tree-key new-tree)
                      '()
                      (tree-right new-tree)))]
            [else
             (values (make-tree
                      (tree-key new-tree)
                      (tree-left new-tree)
                      '())
                     (tree-right new-tree))])))
      
      ;; **************************************************
      ;; insert: key tree -> tree
      ;; insert a new key into a tree
      (define (insert key a-tree)
        (let-values ([(left right) (split key a-tree)])
          (make-tree key left right)))
      
      ;; **************************************************
      ;; delete: key tree -> tree
      ;; delete the node containing key from a tree
      (define (delete key a-tree)
        (let ([new-tree (access key a-tree)])
          (if (same? key (tree-key new-tree))
              (join (tree-left new-tree) (tree-right new-tree))
              new-tree)))
      
      ;; ****************************************************************
      ;; fold-ascending: (alpha beta -> beta) beta (treeof alpha) -> beta
      (define (fold-ascending conser init a-tree)
        (if (null? a-tree) init
            (foldr
             conser
             (conser (tree-key a-tree) (fold-ascending conser init (tree-right a-tree)))
             (fold-ascending cons '() (tree-left a-tree)))))
      
      ;; *****************************************************************
      ;; fold-descending: (alpha beta -> beta) beta (treeof alpha) -> beta
      (define (fold-descending conser init a-tree)
        (if (null? a-tree) init
            (foldr
             conser
             (conser (tree-key a-tree) (fold-descending conser init (tree-left a-tree)))
             (fold-descending cons '() (tree-right a-tree)))))
      
      ;; **************************************************
      ;; tree-for-each!: (alpha beta -> who-cares) (treeof alpha) -> void
      ;; in-order traversal
      (define (tree-for-each! proc a-tree)
        (unless (null? a-tree)
          (tree-for-each! proc (tree-left a-tree))
          (proc (tree-key a-tree))
          (tree-for-each! proc (tree-right a-tree))))
      )))
