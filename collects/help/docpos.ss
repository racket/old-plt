
; Define an order one the standard docs:
(lambda (d)
  (case (string->symbol d)
    [(drscheme) -1]
    [(r5rs) 0]
    [(mzscheme) 1]
    [(mred) 2]
    [(framework) 4]
    [(misclib) 5]
    [(mzc) 10]
    [(insidemz) 50]
    [else 100]))

