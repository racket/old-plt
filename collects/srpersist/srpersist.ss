;; srpersist.ss

(current-require-relative-collection '("srpersist"))

(require-library "macro.ss")
(require-library "cores.ss")
(require-relative-library "srpersistu.ss")

(define-values/invoke-unit/sig
  srpersist:srpersist^
  srpersist@)
