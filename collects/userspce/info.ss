(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what failure)
    (case what
      [(name) "Userspace"]
      [(compile-prefix) (drs 'compile-prefix failure)]
      [(compile-omit-files) (list "userspcs.ss" "ricedefs.ss")]
      [else (failure)])))
