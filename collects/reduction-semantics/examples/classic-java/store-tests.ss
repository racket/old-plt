(module store-tests mzscheme

  (require (lib "test.ss" "test"))
  (provide store-tests)
  (require/expose "store.ss" ())

  (define store-tests
   (make-test-suite "store ADT tests"
     (make-test-case "deref empty store"
       (assert-false (store-ref empty-store 3 (lambda () #f))))

     (make-test-case "allocate into empty store"
       (mv-assert equal?
                  (store-alloc empty-store 'foo)
                  0
                  (store [0 'foo])))

     (make-test-case "allocate into bigger store"
       (mv-assert equal?
                  (store-alloc (store [0 'foo] [1 'bar]) 'baz)
                  2
                  (store [2 'baz] [0 'foo] [1 'bar])))

     (make-test-case "deref from full store"
       (assert-eq? (store-ref (store [0 'foo] [1 'bar]) 1) 'bar))

     (make-test-case "deref bogus address"
       (assert-false (store-ref (store [0 'foo] [1 'bar]) 3 (lambda () #f))))

     (make-test-case "deref bogus address; default fk"
       (assert-exn exn:application:mismatch?
                   (lambda () (store-ref (store [0 'foo] [1 'bar]) 3))))

     (make-test-case "update existing address"
       (assert-equal? (store-update (store [0 'foo] [1 'bar]) 1 'quux)
                      (store [0 'foo] [1 'quux])))

     (make-test-case "update bogus address"
       (assert-exn exn:application:mismatch?
                   (lambda ()
                     (store-update (store [0 'foo] [1 'bar]) 3 'quux)))))))
