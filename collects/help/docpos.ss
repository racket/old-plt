
; Define an order one the standard docs:
(lambda (d)
  (case (string->symbol d)
    [(r5rs) 0]
    [(mzscheme) 1]
    [(mred) 2]
    [(drscheme) 3]
    [(framework) 4]
    [(insidemz) 50]
    [else 100]))

