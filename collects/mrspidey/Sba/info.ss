(lambda (request failure)
  (case request
    [(name) "MrSpidey Sba"]
    [(compile-prefix) '(begin
			 (require-library "sbasig.ss" "mrspidey"))]
    [(compile-omit-files)
     (list "sigs.ss" "exn-hierarchy.ss")]
    [(compile-elaboration-zos) (list "sigs.ss")]
    [(compile-elaboration-zos-prefix) '(begin
					 (require-library "refer.ss")
					 (require-library "cores.ss")
					 (require-library "corem.ss")
					 (require-library "macros.ss" "mrspidey")
					 (require-library "pltrc-co.ss" "mrspidey"))]
    [else (failure)]))
