(module break mzscheme

  (require (lib "contracts.ss")
           (lib "mred.ss" "mred"))
  
  (provide current-breakpoint-handler)
  
  (define (default-current-breakpoint-handler)
    (stop-box "breakpoint-handler not set"
              "The current-breakpoint-handler parameter has not yet been set in this thread."))
  
  (define current-breakpoint-handler
    (make-parameter default-current-breakpoint-handler
                    (lambda (new-handler)
                      (if (and (procedure? new-handler)
                               (procedure-arity-includes? new-handler 0))
                          new-handler
                          (begin
                            (stop-box "Can't set handler"
                                      (format "Bad value for current-breakpoint-handler: ~e" new-handler))
                            default-current-breakpoint-handler)))))
  
  
  (provide/contract [break (-> any)])
  
  (define (break)
    ((current-breakpoint-handler)))
  
  (define (stop-box title message)
    (message-box/custom title
                        message
                        "OK"
                        #f
                        #f
                        #f
                        '(stop))))
  
  