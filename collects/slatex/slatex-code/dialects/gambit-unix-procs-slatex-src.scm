(define delete-file
  (lambda (f)
    (##shell-command (string-append "rm " f))))
