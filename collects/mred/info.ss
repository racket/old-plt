
(lambda (request)
  (case request
    [(name) "MrEd"]
    [(compile-prefix) '(begin
			 (read-case-sensitive #t)
			 (require-library "sig.ss" "mred")
			 (require-library "debug.ss" "system"))]
    [(compile-omit-files)
     (list "sig.ss" "stsigs.ss" "minsig.ss" "minwxsig.ss" "wxs.ss"
	   "wxr.ss" "cppmode.ss")]
    [else (error 'mred-info "Unknown request: ~s" request)]))
