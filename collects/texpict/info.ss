(lambda (sym fail)
  (case sym
    [(name) "TeX Pict"]
    [(help-desk-message)
     (format "Mz/Mr: ~s" `(require-library "texpict.ss" "texpict"))]
    [else (fail)]))
