(module abort-resume mzscheme
  (require "define-closure.ss"
           (lib "serialize.ss"))
  (provide

   ;; AUXILLIARIES
   abort
   resume
   the-cont-key
   abort/cc
   the-undef
   
   ;; "SERVLET" INTERFACE
   start-interaction
   send/suspend
   
   ;; "CLIENT" INTERFACE
   dispatch-start
   dispatch
   )
    
  ;; **********************************************************************
  ;; **********************************************************************
  ;; AUXILLIARIES
  
  (define-struct cont-key ())
  (define the-cont-key (make-cont-key))
  
  ;; BUGBUG this isn't thread safe
  (define current-abort-continuation
    (box #f))
  
  ;; abort: ( -> alpha) -> alpha
  ;; erase the stack and apply a thunk
  (define (abort thunk)
    (let ([abort-k (unbox current-abort-continuation)])
      (abort-k thunk)))
  
  ;; resume: (listof (value -> value)) value -> value
  ;; resume a computation given a value and list of frame procedures
  (define (resume frames val)
    (cond
      [(null? frames) val]
      [else
       (let ([f (car frames)])
         (f (with-continuation-mark the-cont-key f (resume (cdr frames) val))))]))
  
  (define-syntax (abort/cc stx)
    (syntax-case stx ()
      [(_ expr)
       #'((let/cc abort-k
            (set-box! current-abort-continuation abort-k)
            (lambda () expr)))]))
  
  
  ;; a serializable undefined value
  (define-serializable-struct undef ())
  (define the-undef (make-undef))
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; "SERVLET" INTERFACE
  
  (define decode-continuation
    (lambda (k-val)
      (error "interactive module not initialized")))
  
  (define (start-continuation val)
    (error "interactive module not initialized"))
    
  ;; start-interaction: (request -> continuation) -> request
  ;; register the decode proc and start the interaction with the current-continuation
  (define (start-interaction decode)
    (set! decode-continuation decode)
    ((lambda (k0) (abort (lambda () (set! start-continuation k0))))
     (let ([current-marks
            (reverse
             (continuation-mark-set->list (current-continuation-marks) the-cont-key))])
       (lambda (x) (abort (lambda () (resume current-marks x)))))))
    
  (define-closure kont (x) (current-marks)
    (abort (lambda () (resume current-marks x))))
  
  ;; send/suspend: (continuation -> response) -> request
  ;; produce the current response and wait for the next request
  (define (send/suspend response-maker)
    ((lambda (k) (abort (lambda () (response-maker k))))
     (let ([current-marks
            (reverse
             (continuation-mark-set->list (current-continuation-marks) the-cont-key))])
       (make-kont (lambda () current-marks)))))
;       (lambda (x) (abort (lambda () (resume current-marks x)))))))
  
  
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; "CLIENT" INTERFACE
  
  ;; dispatch-start: request -> reponse
  ;; pass the initial request to the starting interaction point
  (define (dispatch-start req0)
    (abort/cc (start-continuation req0)))
  
  ;; dispatch: request -> response
  ;; lookup the continuation for this request and invoke it
  (define (dispatch req)
    (abort/cc
     (cond
       [(decode-continuation req)
        => (lambda (k) (k req))]
       [else
        (error "no continuation associated with the provided request")])))    
  )