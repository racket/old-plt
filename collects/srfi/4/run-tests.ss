(module run-tests mzscheme
  (require "homo-vectors.ss"
           (lib "mz-testing.ss" "tests" "utils"))
  
  
  (SECTION 'HOMOGENEOUS-VECTORS)
  
  ;; test specific numeric properties of the vector classes:
  (define testing-records
    `((,s32vector ,s32vector-ref ,s32vector-set! (,(- (expt 2 31) 1) ,(- (expt 2 31))) (,(expt 2 31) ,(- (+ (expt 2 31) 1))))
      (,s16vector ,s16vector-ref ,s16vector-set! (,(- (expt 2 15) 1) ,(- (expt 2 15))) (,(expt 2 15) ,(- (+ (expt 2 15) 1))))
      (,s8vector ,s8vector-ref ,s8vector-set! (,(- (expt 2 7) 1) ,(- (expt 2 7))) (,(expt 2 7) ,(- (+ (expt 2 7) 1))))
      (,u32vector ,u32vector-ref ,u32vector-set! (,(- (expt 2 32) 1)) (-1 ,(expt 2 32)))
      (,u16vector ,u16vector-ref ,u16vector-set! (,(- (expt 2 16) 1)) (-1 ,(expt 2 16)))
      (,u8vector ,u8vector-ref ,u8vector-set! (,(- (expt 2 8) 1)) (-1 ,(expt 2 8)))))
  
  (map (lambda (x)
         (apply (lambda (maker getter setter good-vals bad-vals)
                  (for-each (lambda (good-val)
                              (test good-val getter (maker good-val) 0)
                              (let ([vec (maker 0)])
                                (setter vec 0 good-val)
                                (test good-val getter vec 0)))
                            good-vals)
                  (for-each (lambda (bad-val)
                              (err/rt-test (maker bad-val) exn:application:type?)
                              (err/rt-test (setter (maker 0) 0 bad-val) exn:application:type?))
                            bad-vals))
                x))
       testing-records)

  ;; test general properties of all homo-vector classes:
(define-syntax (make-type-specific-tests stx)
    (with-syntax ([(_ type-string-stx) stx])
      (let ([type-string (syntax-e #`type-string-stx)])
        #`(let ([vec->homo #,(string->symbol (format "vector->~avector" type-string))]   
                [maker #,(string->symbol (format "~avector" type-string))]
                [homo->vec #,(string->symbol (format "~avector->vector" type-string))]
                [length #,(string->symbol (format "~avector-length" type-string))]
                [getter #,(string->symbol (format "~avector-ref" type-string))]
                [setter #,(string->symbol (format "~avector-set!" type-string))]
                [pred #,(string->symbol (format "~avector?" type-string))]
                [sum #,(string->symbol (format "~avector-sum" type-string))]
                [difference #,(string->symbol (format "~avector-difference" type-string))]
                [scale #,(string->symbol (format "~avector-scale" type-string))]
                [norm #,(string->symbol (format "~avector-norm" type-string))]
                [type-idx #,(string->symbol (format "~avector-type" type-string))]
                [filling-maker #,(string->symbol (format "make-~avector" type-string))]
                [list->homo #,(string->symbol (format "list->~avector" type-string))]
                [homo->list #,(string->symbol (format "~avector->list" type-string))])
            (test #t pred (maker 3 4 5))
            (test #t pred (maker))
            (arity-test pred 1 1)
            (test 3 inexact->exact (getter (maker 3 4 5) 0))
            (test 4 inexact->exact (getter (maker 3 4 5) 1))
            (test 5 inexact->exact (getter (maker 3 4 5) 2))
            (test 3 length (maker 3 4 5))
            (test 0 length (maker))
            (arity-test length 1 1)
            (err/rt-test (length "apple") exn:application:type?)
            (arity-test getter 2 2)
            (err/rt-test (getter "apple" 3) exn:application:type?)
            (err/rt-test (getter (maker 4 5 6) 3) exn:application:mismatch?)
            (err/rt-test (getter (maker) 0) exn:application:mismatch?)
            (err/rt-test (getter (maker) (expt 2 100)) exn:application:mismatch?)
            (err/rt-test (getter (maker 4 5 6) -1))
            (err/rt-test (getter (maker 4 5 6) 2.0))
            (err/rt-test (getter (maker 4 5 6) "2"))
            (let ((vec (maker 4 5 6)))
              (setter vec 1 63)
              (test 63 inexact->exact (getter vec 1)))
            (test `(3 3) map inexact->exact (homo->list (filling-maker 2 3)))
            (test `() homo->list (filling-maker 0))
            (test `() homo->list (filling-maker 0 14))
            (test 2048 length (filling-maker 2048 14))
            (arity-test filling-maker 1 2)
            (err/rt-test (filling-maker "a" 2))
            (err/rt-test (filling-maker 1.0 2))
            (err/rt-test (filling-maker 10.2 2))
            (err/rt-test (filling-maker -1 2))
            (err/rt-test (filling-maker 1000000000000000000000 2) exn:application:type?)
            (arity-test setter 3 3)
            (err/rt-test (setter (maker) 0 'x) exn:application:mismatch?)
            (err/rt-test (setter (maker 1 2 3) -1 'x))
            (err/rt-test (setter (maker 1 2 3) 3 'x) exn:application:mismatch?)
            (err/rt-test (setter (maker 1 2 3) (expt 2 100) 'x) exn:application:mismatch?)
            (err/rt-test (setter '(1 2 3) 2 'x))
            (err/rt-test (setter (maker 1 2 3) "2" 'x))
            (arity-test sum 2 2)
            (arity-test difference 2 2)
            (arity-test norm 1 1)
            (arity-test scale 2 2)
            (test `(3 4 5) map inexact->exact (homo->list (list->homo `(3 4 5))))
            (test `(3 4 5) map inexact->exact (homo->list (maker 3 4 5)))))))


  (make-type-specific-tests "f64")
  (make-type-specific-tests "f32")
  (make-type-specific-tests "s32")
  (make-type-specific-tests "s16")
  (make-type-specific-tests "s8")
  (make-type-specific-tests "u32")
  (make-type-specific-tests "u16")
  (make-type-specific-tests "u8")

  
  (report-errs))