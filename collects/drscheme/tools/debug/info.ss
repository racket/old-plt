(let ([drs (require-library "info.ss" "drscheme")])
  (lambda (what)
    (case what
      [(name) "Debugger Tool"]
      [(compile-prefix) (drs 'compile-prefix)]
      [(compile-omit-files) null]
      [else (error 'tool-info.ss "received unknown flag: ~a~n" what)])))
