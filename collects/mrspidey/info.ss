(lambda (request failure)
  (case request
    [(name) "MrSpidey"]
    [(blurb)
     (list
      "MrSpidey is a static debugger available from DrScheme. "
      "See the "
      `(a ((href ,(format "file:~a" (build-path (collection-path "doc") "mrspidey" "index.html"))))
	  "MrSpidey manual")
      " for more information.")]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "sig.ss" "mred")
			 (require-library "sigs.ss" "mrspidey" "Sba")
			 (require-library "sigs.ss" "mrspidey" "Gui"))]
    [(compile-omit-files)
     (list "pltrc-co.ss" "macros.ss"
	   "handlers.ss" "mred.ss" "text.ss"
	   "sbasig.ss")]
    [(compile-subcollections) (list (list "mrspidey" "Sba")
				    (list "mrspidey" "Gui")
				    (list "drscheme" "tools" "analysis"))]
    [else (failure)]))
