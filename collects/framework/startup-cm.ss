(module startup-cm mzscheme
  (provide install-cm)
  
  (define (install-cm cm-envvar debug-envvar)
    (define debugging? (getenv debug-envvar))
    
    (define install-cm? (and (not debugging?)
                             (getenv cm-envvar)))
    
    (define cm-trace? (or (equal? (getenv cm-envvar) "trace")
                          (equal? (getenv debug-envvar) "trace")))
    
    (when debugging?
      (printf "~a: installing CM to load/create errortrace zos\n" debug-envvar)
      (let-values ([(zo-compile
                     make-compilation-manager-load/use-compiled-handler
                     manager-trace-handler)
                    (parameterize ([current-namespace (make-namespace)])
                      (values
                       (dynamic-require '(lib "zo-compile.ss" "errortrace") 'zo-compile)
                       (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                       (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
        (current-compile zo-compile)
        (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
        (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
        (error-display-handler (dynamic-require '(lib "errortrace-lib.ss" "errortrace")
                                                'errortrace-error-display-handler))
        (when cm-trace?
          (printf "~a: enabling CM tracing\n" debug-envvar)
          (manager-trace-handler
           (lambda (x) (display "1: ") (display x) (newline))))))
    
    (when install-cm?
      (printf "~a: installing compilation manager\n" cm-envvar)
      (let-values ([(make-compilation-manager-load/use-compiled-handler
                     manager-trace-handler)
                    (parameterize ([current-namespace (make-namespace)])
                      (values
                       (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                       (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
        (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
        (when cm-trace?
          (printf "~a: enabling CM tracing\n" cm-envvar)
          (manager-trace-handler
           (lambda (x) (display "1: ") (display x) (newline))))))))
