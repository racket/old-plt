(lambda (sym fail)
  (case sym
    [(name) "Texpict"]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "sig.ss" "mred")
			 (require-library "texpicts.ss" "texpict")
			 (require-library "mrpicts.ss" "texpict")
			 (require-library "utilss.ss" "texpict"))]
    [(compile-omit-files) (list "commons.ss" "texpicts.ss" "mrpicts.ss" "utilss.ss")]
    [(help-desk-message)
     (format "Mz/Mr: ~s" `(require-library "texpict.ss" "texpict"))]
    [(mred-launcher-libraries) (list "slideshow.ss")]
    [(mred-launcher-names) (list "Slideshow")]
    [else (fail)]))
