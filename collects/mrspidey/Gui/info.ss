(lambda (request failure)
  (case request
    [(name) "MrSpidey Gui"]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "sig.ss" "mred")
			 (require-library "sigs.ss" "mrspidey" "Sba")
			 (require-library "sigs.ss" "mrspidey" "Gui")
			 (require-library "macros.ss" "mrspidey")
			 (require-library "pltrc-co.ss" "mrspidey"))]
    [(compile-omit-files)
     (let ([f (normal-case-path "loadu.ss")])
       (let loop ([l (directory-list (collection-path "mrspidey" "Gui"))])
	 (if (string=? (normal-case-path (car l)) f)
	     (cdr l)
	     (cons (car l) (loop (cdr l))))))]
    [else (failure)]))
