;; info.ss for srpersist collection

;; no .zo compilation necessary, since all the
;; real code is in C++

(lambda (request failure-thunk)
  (case request
    [(name) "SrPersist"]
    [(compile-prefix) void]
    [(compile-omit-files) 
      '("info.ss"
	"sigs.ss"
	"invoke-1.0.ss"
	"invoke-2.0.ss"
	"invoke-3.0.ss"
	"invoke-3.5.ss"
	"srpersist.ss"
        "srpersistu.ss")]
    [(blurb)
     (list
      "SrPersist is an extension for using ODBC databases. " 
      "See the "
      `(a ((href ,(format "file:~a" (build-path (collection-path "srpersist") "doc.txt"))))
      "doc.txt")
      " file for more information.")]
    [else (failure-thunk)]))
