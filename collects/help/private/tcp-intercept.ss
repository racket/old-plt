(module tcp-intercept mzscheme
  (provide tcp-intercept@)
  
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "sig.ss" "web-server")
           (lib "tcp-sig.ss" "net")
           "internal-hp.ss")
  
  (define raw:tcp-abandon-port tcp-abandon-port)
  (define raw:tcp-accept tcp-accept) 
  (define raw:tcp-accept-ready? tcp-accept-ready?)
  (define raw:tcp-addresses tcp-addresses)
  (define raw:tcp-close tcp-close)
  (define raw:tcp-connect tcp-connect)
  (define raw:tcp-connect/enable-break tcp-connect/enable-break)
  (define raw:tcp-listen tcp-listen)
  (define raw:tcp-listener? tcp-listener?)
  
  ; For tcp-listeners, we use an else branch in the conds since
  ; (instead of a contract) I want the same error message as the raw
  ; primitive for bad inputs.
  
  ; : (listof nat) -> (unit/sig () -> net:tcp^)
  (define tcp-intercept@
    (unit/sig net:tcp^
      (import web-server^)
      
      ; : port -> void
      (define (tcp-abandon-port tcp-port)
        (cond
          [(tcp-port? tcp-port)
           (raw:tcp-abandon-port tcp-port)]
          [(input-port? tcp-port)
           (close-input-port tcp-port)]
          [(output-port? tcp-port)
           (close-output-port tcp-port)]
          [else (void)]))
      
      ; : listener -> iport oport
      (define tcp-accept raw:tcp-accept)
      
      ; : tcp-listener -> iport oport
      (define tcp-accept-ready? raw:tcp-accept-ready?)
      
      ; : tcp-port -> str str
      (define (tcp-addresses tcp-port)
        (if (tcp-port? tcp-port)
            (raw:tcp-addresses tcp-port)
            (values "127.0.0.1" internal-host)))
      
      ; : port -> void
      (define tcp-close raw:tcp-close)
      
      ; : (str nat -> iport oport) -> str nat -> iport oport
      (define (gen-tcp-connect raw)
        (lambda (hostname-string port)
          (if (and (string=? internal-host hostname-string)
                   (equal? internal-port port))
              (let-values ([(req-in req-out) (make-pipe)]
                           [(resp-in resp-out) (make-pipe)])
                (parameterize ([current-custodian (make-custodian)])
                  (serve-ports req-in resp-out))
                (values resp-in req-out))
              (raw hostname-string port))))
      
      ; : str nat -> iport oport
      (define tcp-connect (gen-tcp-connect raw:tcp-connect))
      
      ; : str nat -> iport oport
      (define tcp-connect/enable-break (gen-tcp-connect raw:tcp-connect/enable-break))
      
      ; FIX - support the reuse? flag.
      (define tcp-listen raw:tcp-listen)
      
      ; : tst -> bool
      (define tcp-listener? raw:tcp-listener?))))
