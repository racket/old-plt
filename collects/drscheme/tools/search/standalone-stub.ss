(require-library "cores.ss")
(define-values/invoke-unit/sig 
 mzlib:core^
 (require-library "corer.ss"))
(require-library "framework.ss" "framework")

(define (drscheme:frame:basics-mixin x) x)

(load-relative "multi-file-search.ss")

(multi-file-search)
