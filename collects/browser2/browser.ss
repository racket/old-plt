(require-library "sig.ss" "browser2")

(define-values/invoke-unit/sig browser2^
  (unit/sig browser2^
    (import mred^)

    (include "comma-delimited-text-parser.ss")
    (include "utils.ss")
    (include "render-html.ss")
    (include "render-table.ss")))

