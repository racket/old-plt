(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what failure)
    (case what
      [(name) "Graphic Userspace"]
      [(compile-prefix) (drs 'compile-prefix failure)]
      [(compile-omit-files) null]
      [else (failure)])))
