
;; option.ss
;; (c) 1996 Sebastian Good

(unit/sig
 compiler:option^
 (import)

 (define propagate-constants (make-parameter #t))
 (define assume-primitives (make-parameter #f))
 (define stupid (make-parameter #f))
 
 (define vehicles (make-parameter 'vehicles:automatic))
 (define vehicles:monoliths (make-parameter 1))
 (define seed (make-parameter 2001))
 (define max-monoliths 32)
 
 (define verbose (make-parameter #f))
 (define debug (make-parameter #f))
 (define test (make-parameter #f))
 (define clean-intermediate-files (make-parameter #t))
 
 (define max-exprs-per-top-level-set (make-parameter 25))
 
 (define setup-prefix (make-parameter ""))
 
 ;; Maybe #f helps for register-poor architectures?
 (define unpack-environments (make-parameter #t)))

