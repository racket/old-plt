(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what failure)
    (case what
      [(name) "Project Manager Tool"]
      [(compile-prefix) (drs 'compile-prefix failure)]
      [(compile-omit-files) null]
      [else (failure)])))
