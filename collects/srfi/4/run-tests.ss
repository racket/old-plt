(module run-tests mzscheme
  (require "homo-vectors.ss")
  
  (load (build-path "/Users/clements/plt" "tests" "mzscheme" "testing.ss"))
  
  (define SECTION (namespace-variable-value 'SECTION))
  (define test (namespace-variable-value 'test))
  (define arity-test (namespace-variable-value 'arity-test))
  (define thunk-error-test (namespace-variable-value 'thunk-error-test))
  (define report-errs (namespace-variable-value 'report-errs))
  
  (define-syntax err/rt-test
    (lambda (stx)
      (syntax-case stx ()
        [(_ e exn?)
         (syntax
          (thunk-error-test (lambda () e) (quote-syntax e) exn?))]
        [(_ e)
         (syntax
          (err/rt-test e exn:application:type?))])))
  
  
  (SECTION 'HOMOGENEOUS-VECTORS)
  
  ;; test general properties of all homo-vector classes:
  (run-homo-vec-tests)
  
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
  
  (report-errs))