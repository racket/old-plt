(lambda (sym fail)
  (case sym
    [(name) "Texpict"]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "sig.ss" "mred")
			 (require-library "texpicts.ss" "texpict")
			 (require-library "mrpicts.ss" "texpict"))]
    [(compile-omit-files) (list "commons.ss" "texpicts.ss" "mrpicts.ss")]
    [(help-desk-message)
     (format "Mz/Mr: ~s" `(require-library "texpict.ss" "texpict"))]
    [else (fail)]))
