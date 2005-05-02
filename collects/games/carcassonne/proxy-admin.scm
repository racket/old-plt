#cs
(module proxy-admin mzscheme
  
  ;; the proxy administrator
  ;;   it serves in the role of a game administrator on the CLIENT side 
  ;;   it forwards its messages to the proxy-player on the SERVER side
  
  ;; TODO: check for '(error 
  ;;   it is a protocol error and I think I should signal a contract violation 
  
  (require "if.scm" "contract.scm" "proxy-auxs.scm" (lib "class.ss"))
  
  (provide
   proxy-admin% ;; admin<%>
   )
  
  (define proxy-admin% 
    (class* object% (admin<%>)
      (super-new)
      
      (init-field input output)
      
      (define (cb0 m args) (error 'proxy-admin "no callback available: ~a" m))
      
      (define (err-eof) 
        (error 'proxy-admin "server closed the connection prematurely"))
      
      (define (err-bad-msg r)
        (error 'proxy-admin "server sent an unknown message ~e" r))
      
      (field 
       [listen (make-listen input cb0 err-eof err-bad-msg)]
       [call   (make-call output)]
       [return (make-return output)])
      
      (define/public (register p)
        (call 'register (get-field name p))
        (let ([r (listen)])
          (if (boolean? r)
              #f
              (begin
                (set! the-player p)
                r))))
      
      (define/public (run-game)         
        (define (player:closure method args)
          (case method
            [(take-turn) 
             (return (send the-player take-turn (new proxy-turn%)))]
            [(inform)    
             ;; (= (length args) 1)
             (check-args 'inform 1 args)
             (return (send the-player inform (car args)))]
            [(score-and-token)
             ;; (= (length args) 2)
             (check-args 'score-and-token 2 args)
             (return (send the-player score-and-token (car args) (cadr args)))]
            [(other-score-and-token)
             ;; (= (length args) 3)
             (check-args 'other-score-and-token 3 args)
             (return (send/apply the-player other-score-and-token args))]
            [(placed-tile)
             ;; (= (length args) 1)
             (check-args 'placed-tile 1 args)
             (return (send/apply the-player placed-tile args))]
            [(placed-follower)
             (check-args 'placed-follower 3 args)
             (return (send/apply the-player placed-follower args))]            
            [else (error 'proxy-admin "unknown call: ~e" `(,method . ,args))]))
        (unless (boolean? the-player)
          (thread 
           (lambda ()
             (with-handlers 
                 ([contract? (lambda (x) (error 'client (contract-msg x)))]
                  [exn:fail? (lambda (x) (error 'client (exn-message x)))])
               (listen player:closure))))))
      
      ;; (union false player<%>)
      ;; the registered player, if registration succeeded
      (define the-player #f)
      
      (define proxy-turn%
        (class* object% (turn<%>)
          (super-new)
          
          (define/public (get-index)
	    (call 'get-index)
	    (listen))
          
          (define/public (potential-locations-for-tile)
            (call 'potential-locations-for-tile)
            (listen))
          
          (define/public (place-tile t)
            (define (player:closure method args)
              (if (eq? 'score-and-token method) ;; (>= (length args) 2)
                  (begin 
                    (check-args 'score-and-token 2 args)
                    (send the-player score-and-token (car args) (cadr args)))
                  '(error 'place-tile "bad protocol: ~e" `(,method . ,args)))
              (return (void)))
            (call 'place-tile t)
            (listen player:closure))
          
          (define/public (potential-locations-for-followers) 
            (call 'potential-locations-for-followers)
            (listen))
          
          (define/public (place-follower t p) 
            (call 'place-follower t p)
            (listen))))
      
      (define/public (register-observer o) 
        (error 'proxy-admin "register-observer not implemented"))
      
      (define/public (get-graph) 
        (error 'proxy-admin "get-graph not implemented"))
      
      ))
  (new proxy-admin%
       [input (open-input-string "hello world")]
       [output (open-output-string)])
  
  )
