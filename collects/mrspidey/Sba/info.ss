(lambda (request failure)
  (case request
    [(name) "MrSpidey Sba"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "pltrc-co.ss" "mrspidey")
			 (require-library "macros.ss" "mrspidey")
			 (require-library "sigs.ss" "mrspidey" "Sba"))]
    [(compile-omit-files)
     (let ([fs (map normal-case-path '("loadu.ss"
				       "link.ss"
				       "zod-extra.ss"
				       "zod-link.ss"))])
       (let loop ([l (directory-list (collection-path "mrspidey" "Sba"))])
	 (if (null? l)
	     null
	     (if (member (normal-case-path (car l)) fs)
		 (loop (cdr l))
		 (cons (car l) (loop (cdr l)))))))]
    [(compile-elaboration-zos) (list "sigs.ss")]
    [(compile-elaboration-zos-prefix) '(begin
					 (read-case-sensitive #t)
					 (require-library "refer.ss")
					 (require-library "wxs.ss" "system")
					 (require-library "sig.ss" "mred")
					 (require-library "drsig.ss" "drscheme"))]
    [else (failure)]))
