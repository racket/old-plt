#cs
(module contract mzscheme

  ;; a simplistic framework for signaling contract violations 

  (require (file "Testing/testing.scm"))
  
  (provide 
   contract?;; Any -> Boolean
   qos-contract? ;; Any -> Boolean
   contract-msg ;; Contract -> String 
   ;; extract message from contract
   contract/violation ;; X -> Void 
   ;; raise an exception that indicates a contract violation 
   sequence/violation ;; X -> Void 
   ;; raise an exception that indicates a sequence contract violation 
   assert ;; (X -> Boolean) X FormatString[X] -> X 
   ;; does the given X satisfy the given predicate? 
   qos/violation ;; X -> Void 
   ;; raise an excpetion when the quality of service was subpar 
   )
   
  
  (define CONTRACT 'contract)
  (define SEQUENCE 'sequence)
  (define QoS 'qos)
  (define tags (list CONTRACT SEQUENCE QoS))
  
  (define (contract? x) (and (pair? x) (or (memq (car x) tags))))
  (define (qos-contract? x) (and (pair? x) (eq? (car x) QoS)))
  
  (define make-contract cons)
  (define contract-msg cdr)

  (define (contract/violation x) (raise (make-contract CONTRACT x)))
  
  (define (sequence/violation x) (raise (make-contract SEQUENCE x)))
  
  (define (qos/violation x) (raise (make-contract QoS x)))
  
  (define (assert p x msg) (if (p x) x (contract/violation (format msg x))))
  
  (test-e (contract/violation 10) contract)
  (test-e (sequence/violation 10) contract)
  

  )
