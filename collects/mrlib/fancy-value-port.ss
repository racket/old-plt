(module fancy-value-port mzscheme
  (require (lib "pretty.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  (provide install-fancy-value-port-display-handler
           install-fancy-value-port-write-handler
           install-fancy-value-port-print-handler)
  
  (define orig-pp-parameters (current-parameterization))
  
  (define (install-fancy-value-port-display-handler port)
    (let ([original-port-display-handler (port-display-handler port)])
      (port-display-handler
       (λ (port val)
         (cond
           [(string? val) (original-port-display-handler port val)]
           [else
            (do-printing pretty-display port val)])))))
  
  (define (install-fancy-value-port-write-handler port)
    (port-write-handler
     (λ (port val)
       (do-printing pretty-print port val))))
  
  (define (install-fancy-value-port-print-handler port)
    (port-write-handler
     (λ (port val)
       (do-printing pretty-print port val))))
            
  (define (use-number-snip? x)
    (and (number? x)
         (exact? x)
         (real? x)
         (not (integer? x))))
  
  (define (do-printing pretty port value)
    (call-with-parameterization 
     orig-pp-parameters
     (parameterize ([pretty-print-size-hook
                     (λ (value display? port)
                       (cond
                         [(is-a? value snip%) 1]
                         [(use-number-snip? value) 1]
                         [(syntax? value) 1]
                         [else #f]))]
                    [pretty-print-print-hook
                     (λ (value display? port)
                       (cond
                         [(is-a? value snip%)
                          (write-special value port)
                          1]
                         [(use-number-snip? value)
                          '(write-special
                           (number-snip:make-repeating-decimal-snip value #f)
                           port)
                          1]
                         [(syntax? value)
                          '(write-special (render-syntax/snip value))]
                         [else (void)]))])
       (pretty value port)))))
