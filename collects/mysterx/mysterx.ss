;; mysterx.ss

(require-library "macro.ss")

(require-library "sigs.ss" "mysterx")
(require-library "mysterxu.ss" "mysterx")

(define-values/invoke-unit/sig
  mysterx:mysterx^
  mysterx@)
