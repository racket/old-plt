(module test-server02 mzscheme
  (require (lib "unitsig.ss")
           "server-kernel.ss")
  
  (provide test-serve)
  
  
  (define kernel-config@
    (unit/sig server-kernel-config^
      (import)
      (define max-waiting 40)
      (define listen-ip #f)
      (define port 9000)
      (define initial-time-to-live 60)
      
      ;; dispatch: alpha -> path
      (define (dispatch ignored)
        (string->path "dynamic-server.ss"))))
  
  (define (test-serve)
    (serve kernel-config@)))
