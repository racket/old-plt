;; mysterx.ss

(current-require-relative-collection '("mysterx"))

(require-library "macro.ss")
(require-library "cores.ss")
(require-relative-library "mysterxu.ss")

(define-values/invoke-unit/sig
  mysterx:mysterx^
  mysterx@)
