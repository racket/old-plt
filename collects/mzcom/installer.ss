(module installer mzscheme
  (require (lib "process.ss"))
  (provide post-installer)
  (define (post-installer plt-home)
    (let* ([exe-dir  (build-path plt-home "collects" "mzcom")]
           [exe-path (build-path exe-path "mzcom.exe")])
      (cond [(not (eq? (system-type) 'windows))
             (fprintf (current-error-port)
                      "Warning: can't install MzCOM on non-Windows machine\n")]
            [(not (file-exists? exe-path))
             (fprintf (current-error-port)
                      "Warning: MzCOM binary not installed\n")]
            [else (parameterize ([current-directory exe-dir])
                    (system "mzcom.exe /RegServer"))]))))
