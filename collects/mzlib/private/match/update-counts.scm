;; This library is used by match.ss
;; This requires the test data structure.

(define-values (update-counts)
  (letrec 
      (
       ;;!(function test-filter
       ;;          (form (test-filter test-list) -> test-list)
       ;;          (contract list -> list))
       ;; This function filters out tests that do not need to be to have
       ;; their counts updated for reordering purposes.  These are the
       ;; more complex patterns such as or-patterns or ddk patterns.
       (test-filter
        (lambda (tlist)
          (if (null? tlist)
              '()
              (if (= -1 (test-times-used (car tlist)))
                  (test-filter (cdr tlist))
                  (cons (car tlist)
                        (test-filter (cdr tlist)))))))

       ;;!(function update-count
       ;;          (form (update-count test tests-rest pos) -> void)
       ;;          (contract (test-struct list integer) -> void))
       ;; This function updates the test-times-used and test-used-set!
       ;; fields of the test structs.  These fields are essential to
       ;; determining the order of the tests.
       (update-count 
        (lambda (test tests-rest pos)
          (let loop ((l tests-rest)
                     (p (add1 pos)))
            (if (null? l)
                '()
                (begin
                  (when (in (test-tst test) (car l))
                    (begin
                     (set-test-times-used! test (add1 (test-times-used test)))
                     (set-test-used-set! test (cons p (test-used-set test)))))
                  (loop (cdr l) (add1 p)))))))

       ;;!(function update-counts
       ;;          (form (update-counts render-list) -> void)
       ;;          (contract list -> void))
       ;; This function essentially calls update-count on every test in
       ;; all of the test lists.
       (update-counts 
        (lambda (render-list)
          (let* ((test-master-list (map test-filter 
                                        (map car render-list)))
                 (test-so-far-lists
                  (map
                   (lambda (tl)
                     (let ((f (map test-tst (test-filter tl))))
                       (append f (map implied f))))
                   test-master-list)))
            (let loop ((tml test-master-list)
                       (tsf test-so-far-lists)
                       (pos 1))
              (if (null? tml)
                  '()
                  (begin
                    (map (lambda (t)
                           (set-test-times-used! t 1)
                           (set-test-used-set! 
                            t 
                            (cons pos (test-used-set t)))
                           (update-count t (cdr tsf) pos))
                         (car tml))
                    (loop (cdr tml) (cdr tsf) (add1 pos))))))))
       )
    (values update-counts)))


