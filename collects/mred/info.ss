(lambda (request failure)
  (case request
    [(name) "MrEd Collection"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "refer.ss")
			 (require-library "wxs.ss" "system")
			 (require-library "sig.ss" "mred")
			 (require-library "debug.ss" "system"))]
    [(compile-omit-files)
     (list "sig.ss" "stsigs.ss" "minsig.ss" "minwxsig.ss" "wxs.ss"
	   "wxr.ss" "cppmode.ss" "linkwx.ss"
	   "exapp.ss")]
    [else (failure)]))
