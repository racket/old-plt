(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what failure)
    (case what
      [(name) "Userspace"]
      [(compile-prefix) (drs 'compile-prefix)]
      [(compile-omit-files) null]
      [else (failure)])))
