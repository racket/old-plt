(module splay-tree-tests mzscheme
  (require (lib "unitsig.ss")
           "test-harness.ss"
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


  (test "splay-test 01"
        (lambda ()
          (equal?
           (list 1 2 3 4 5)
           (tree-map
            (lambda (x) x)
            (insert 3 (insert 2 (insert 4 (insert 5 (insert 1 '())))))))))

  (test "splay-test 02"
        (lambda ()
          (equal? '(1 2 3 4 4) (tree-map id (list->tree '(1 4 2 4 3))))))

  (test "splay-test 03"
        (lambda ()
          (equal? '() (tree-map id (list->tree '())))))

  (test "splay-test 04"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree-map id t1))))

  (test "splay-test 05"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree-map id t2))))

  (test "splay-test 06"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree-map id t3))))
  (test "splay-test 07"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree-map id t4))))

  (test "splay-test 08"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree-mapr id t1))))

  (test "splay-test 09"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree-mapr id t2))))

  (test "splay-test 10"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree-mapr id t3))))

  (test "splay-test 11"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree-mapr id t4))))

  (test "splay-test 12"
        (lambda ()
          (equal? '() (tree->ascending (list->tree '())))))

  (test "splay-test 13"
        (lambda ()
          (equal? '() (tree->descending (list->tree '())))))

  (test "splay-test 14"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree->ascending t1))))

  (test "splay-test 15"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree->ascending t2))))

  (test "splay-test 16"
        (lambda ()
          (equal? '(1 2 3 4 5) (tree->ascending t3))))

  (test "splay-test 17"
        (lambda ()
          (equal? '(1 2 3  4 5) (tree->ascending t4))))

  (test "splay-test 18"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree->descending t1))))

  (test "splay-test 19"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree->descending t2))))

  (test "splay-test 20"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree->descending t3))))

  (test "splay-test 21"
        (lambda ()
          (equal? '(5 4 3 2 1) (tree->descending t4))))

  (test "splay-test 22"
        (lambda ()
          (equal? '(5 5 5 5 5) (tree-map id (list->tree '(5 5 5 5 5))))))

  (test "splay-test 23"
        (lambda ()
          (test-split01 '(5) '(6))))

  (test "splay-test 24"
        (lambda ()
          (test-split01 '(5 5) '(6))))

  (test "splay-test 25"
        (lambda ()
          (test-split01 '(5 5 5 5 5) '(6))))

  (test "splay-test 26"
        (lambda ()
          (test-split01 '(5 5 5 5 5) '(6 6 6 6 6 6))))

  (test "splay-test 27"
        (lambda ()
          (equal? '(1 2 3 4 5)
                  (tree-map id (delete 6 (delete 6 (delete 6 (delete 6 (delete 6 (delete 6 (list->tree '(1 2 3 4 5 6 6 6 6 6 6))))))))))))

  )
