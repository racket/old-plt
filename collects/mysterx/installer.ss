(module installer mzscheme
  (require (lib "process.ss"))

  (define (post-installer mx-path)
    (define (make-dll-path . more)
      (apply build-path (collection-path "mysterx")
             "private" "compiled" "native" "win32" "i386" more))
    (define (warn fmt . args) (apply fprintf (current-error-port) fmt args))
    (let* ([dlls '("myspage.dll" "myssink.dll")]
           [dll-paths (map make-dll-path dlls)]
           [winsys-dir (find-system-path 'sys-dir)])
      (cond
       [(not (eq? (system-type) 'windows))
        (warn "Warning: can't install MysterX on non-Windows machine\n")]
       [(not (andmap file-exists? dll-paths))
        (warn "Warning: MysterX binaries not installed\n")]
       [(not winsys-dir)
        (warn "Warning: Can't run REGSVR32 on libraries\n")]
       [else (parameterize ((current-directory (make-dll-path)))
               (for-each
                (lambda (dll)
                  (if (system (format "~s /s ~a" ; /s = silent mode
                                      (build-path winsys-dir "REGSVR32.EXE")
                                      dll))
                    (printf "MysterX: Registered library ~a\n" dll)
                    (warn "MysterX: Unable to register library ~a\n" dll)))
                dlls))])))
  (provide post-installer))
