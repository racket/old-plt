'(current-load
 (let ([ol (current-load)])
  (lambda (f)
    (printf "loading ~a~n" f)
    (ol f))))

(require-library "sig.ss" "countdwn")

(invoke-unit/sig (require-library "link.ss" "countdwn"))
