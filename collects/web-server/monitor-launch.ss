; The main program of the "web-server-monitor" launcher. 
(module monitor-launch mzscheme
  (require "monitor-server.ss")

  (define argv (namespace-variable-binding 'argv))

  (if (procedure-arity-includes? monitor (vector-length argv))
      (let ([args (vector->list argv)])
        (apply monitor (car args) (cadr args) (map string->number (cddr args))))
      (error 'web-server-monitor
             "expects alert-email host-name [port] [poll-frequency-seconds] [server-response-timeout-seconds]"))
  
  (semaphore-wait (make-semaphore)))
