(module check-text mzscheme
  (require "private/text-defs.ss" "private/go-check.ss" (lib "unitsig.ss"))
  (provide check-version)
  (define (check-version)
    ;; no frame, always synchronous
    (go-check #f #f text-defs@)))
