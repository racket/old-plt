'(current-load
 (let ([ol (current-load)])
  (lambda (f)
    (printf "loading ~a~n" f)
    (ol f))))

(require-library "sig.ss" "countdwn")

(invoke-open-unit/sig (require-library "link.ss" "countdwn") #f)
