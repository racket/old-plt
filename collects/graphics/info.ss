(let ([drs (require-library "info.ss" "drscheme")])
  (let ([graphics-info
          (lambda (what failure)
            (case what
              [(name) "Graphics"]
              [(compile-prefix) (drs 'compile-prefix failure)]
              [(compile-omit-files) null]
              [else (failure)]))])
    graphics-info))
