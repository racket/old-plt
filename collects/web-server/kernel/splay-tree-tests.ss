(require (lib "unitsig.ss")
         "splay-tree.ss")

(define same? =)
(define less? <)

(define-values/invoke-unit/sig
   splay-tree^
   splay-tree@
   #f comparable^)

(define (list->tree a-list)
  (cond
    [(null? a-list) '()]
    [else
     (insert
      (car a-list)
      (list->tree (cdr a-list)))]))

(define (extract-min a-tree)
  (let* ([a-tree (smallest a-tree)]
         [min (tree-key a-tree)])
    (values min (delete min a-tree))))

(define (extract-max a-tree)
  (let* ([a-tree (largest a-tree)]
         [max (tree-key a-tree)])
    (values max (delete max a-tree))))

(define (tree->ascending a-tree)
  (cond
    [(null? a-tree) '()]
    [else
     (let-values ([(min rest) (extract-min a-tree)])
       (cons min (tree->ascending rest)))]))

(define (tree->descending a-tree)
  (cond
    [(null? a-tree) '()]
    [else
     (let-values ([(max rest) (extract-max a-tree)])
       (cons max (tree->descending rest)))]))

(define (tree-map proc a-tree)
  (fold-ascending
   (lambda (first rest)
     (cons (proc first) rest))
   '()
   a-tree))

(define (tree-mapr proc a-tree)
  (fold-descending
   (lambda (first rest)
     (cons (proc first) rest))
   '()
   a-tree))

(define (id x) x)
(define-values (t1 t2 t3 t4)
  (values
   (list->tree '(5 4 3 2 1))
   (list->tree '(5 3 4 1 2))
   (list->tree '(1 2 3 4 5))
   (list->tree '(1 4 2 5 3))))

(define (test-split01 5-list 6-list)
  (let-values ([(fives sixes) (split 5 (list->tree (append 5-list 6-list)))])
    (and (equal? 5-list (tree-map id fives))
         (equal? 6-list (tree-map id sixes)))))


(equal?
 (list 1 2 3 4 5)
 (tree-map
  (lambda (x) x)
  (insert 3 (insert 2 (insert 4 (insert 5 (insert 1 '())))))))

(equal? '(1 2 3 4 4) (tree-map id (list->tree '(1 4 2 4 3))))

(equal? '() (tree-map id (list->tree '())))
(equal? '(1 2 3 4 5) (tree-map id t1))
(equal? '(1 2 3 4 5) (tree-map id t2))
(equal? '(1 2 3 4 5) (tree-map id t3))
(equal? '(1 2 3 4 5) (tree-map id t4))
(equal? '(5 4 3 2 1) (tree-mapr id t1))
(equal? '(5 4 3 2 1) (tree-mapr id t2))
(equal? '(5 4 3 2 1) (tree-mapr id t3))
(equal? '(5 4 3 2 1) (tree-mapr id t4))


(equal? '() (tree->ascending (list->tree '())))
(equal? '() (tree->descending (list->tree '())))
(equal? '(1 2 3 4 5) (tree->ascending t1))
(equal? '(1 2 3 4 5) (tree->ascending t2))
(equal? '(1 2 3 4 5) (tree->ascending t3))
(equal? '(1 2 3 4 5) (tree->ascending t4))

(equal? '(5 4 3 2 1) (tree->descending t1))
(equal? '(5 4 3 2 1) (tree->descending t2))
(equal? '(5 4 3 2 1) (tree->descending t3))
(equal? '(5 4 3 2 1) (tree->descending t4))

(equal? '(5 5 5 5 5) (tree-map id (list->tree '(5 5 5 5 5))))

(test-split01 '(5) '(6))
(test-split01 '(5 5) '(6))
(test-split01 '(5 5 5 5 5) '(6))
(test-split01 '(5 5 5 5 5) '(6 6 6 6 6 6))

(equal? '(1 2 3 4 5)
        (tree-map id (delete 6 (delete 6 (delete 6 (delete 6 (delete 6 (delete 6 (list->tree '(1 2 3 4 5 6 6 6 6 6 6))))))))))

