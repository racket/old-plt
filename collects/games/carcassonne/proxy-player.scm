#cs
(module proxy-player mzscheme
  
  ;; a proxy player for the SERVER side (which runs the true administrator)
  
  ;; the proxy player forwards method calls from the admin to the CLIENT
  ;; & listens to requests from the client and forwards those to the administrator

  (require "if.scm" "contract.scm" "proxy-auxs.scm" (lib "class.ss"))
  
  (provide proxy-player%)
  
  (define proxy-player% 
    (class* object% (player<%>)
      (super-new)
      
      (init-field input output name [on-time-error void])
      
      (define (cb0 m r)
        (sequence/violation 
         (format "proxy-player: no callback available for ~a" m)))
      
      (define (err-eof)
        (qos/violation
         "the player send garbled message or closed the connection prematurely (see log)"))
      
      (define (err-bad-msg r)
        (sequence/violation 
         (string-append 
          "proxy-player: the remote player sent a message"
          (format "that is neither a call nor a return: ~e" r))))
      
      (field 
       [listen (make-listen input cb0 err-eof err-bad-msg)]
       [call   (make-call output)]
       [return (make-return output)])
      
      (define/public (take-turn t) 
        (define (tile? x) (is-a? x tile<%>))
        (define (turn-as-cb m args) 
          (case m
	    [(get-index)
             ; (check-args 'potential-locations-for-tile 0 args)
             (return (string->number (send t get-index)))]
            [(potential-locations-for-tile)
             ; (check-args 'potential-locations-for-tile 0 args)
             (let ([x (send t potential-locations-for-tile)])
               (return (send t potential-locations-for-tile)))]
            [(place-tile)
             (check-args 'place-tile 1 args)
             (check-type 'place-tile 'tile tile? (car args)) ; 
             (send t place-tile (car args))
             (return (void))]
            [(potential-locations-for-followers)
             ; (check-args 'potential-locations-for-followers 0 args)
             (return (send t potential-locations-for-followers))]
            [(place-follower)
             (check-args 'place-follower 2 args)
             (check-type 'place-follower 'tile tile? (car args))
             (check-type 'place-follower 'position position? (cadr args))
             (return (send t place-follower (car args) (cadr args)))]
            [else (sequence/violation 
                   (string-append
                    (format "expected one of these calls"
                            '(potential-locations-for-tile
                              potential-locations-for-followers
                              place-tile
                              place-follower))
                    "; given: ~e "
                    m))]))
        (define (action)
          (call 'take-turn)
          (listen turn-as-cb))
        (timed-action action (max-turn-time) on-time-error))
      
      ;; Number Number -> Void 
      (define/public (score-and-token s n) 
        (define (action)
          (call 'score-and-token s n)
          (listen))
        (timed-action action MAX-RETURN-TIME on-time-error))
      
      ;; String -> Void 
      ;; accept message and forward it 
      (define/public (inform s) 
        (define (action)
          (call 'inform s)
          (listen))
        (timed-action action MAX-RETURN-TIME on-time-error))
      
      ;; --- the observer interface --- 
      
      ;; Follower Number Number -> Void 
      ;; player with follower f scored s and received n tokens back 
      (define/public (other-score-and-token f s n) 
        (define (action)
          (call 'other-score-and-token f s n)
          (listen))
        (timed-action action MAX-RETURN-TIME on-time-error))
      
      ;; tile<%> -> Void 
      ;; some other player placed t during a turn 
      (define/public (placed-tile t) 
        (define (action)
          (call 'placed-tile t)
          (listen))
        (timed-action action MAX-RETURN-TIME on-time-error))
      
      ;; tile<%> Follower Position -> Void 
      ;; some other player placed f on t at p 
      (define/public (placed-follower t f p)
        (define (action) 
          (call 'placed-follower t f p)
          (listen))
        (timed-action action MAX-RETURN-TIME on-time-error))))
  
  ;; (new proxy-player% [input 'i][output 'o][name "n"])
  
  )
