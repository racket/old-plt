;; this writes values to strings and compares the strings
;; to implements an equal? predicate that works for cyclic
;; structures. When/if equal? works for cyclic stuff, this
;; should be changed to equal? directly.

(load-relative "loadtest.ss")

(SECTION 'shared)

(require (lib "shared.ss"))

(load-relative "shared-tests.ss")
  
(require mzscheme)
(load-relative "shared-tests.ss")
  
(report-errs)
