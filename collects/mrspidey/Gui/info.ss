(lambda (request failure)
  (case request
    [(name) "MrSpidey Gui"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "pltrc-co.ss" "mrspidey")
			 (require-library "macros.ss" "mrspidey")
			 (require-library "sigs.ss" "mrspidey" "Sba")
			 (require-library "sigs.ss" "mrspidey" "Gui"))]
    [(compile-omit-files)
     (let ([f (normal-case-path "loadu.ss")])
       (let loop ([l (directory-list (collection-path "mrspidey" "Gui"))])
	 (if (string=? (normal-case-path (car l)) f)
	     (cdr l)
	     (cons (car l) (loop (cdr l))))))]
    [else (failure)]))
