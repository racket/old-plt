;; info.ss for mysterx collection

(lambda (request failure-thunk)
  (case request
    [(name) "MysterX"]
    [(compile-prefix) 
     '(begin
	(current-require-relative-collection (list "mysterx"))
	(require-library "macro.ss")
	(require-library "cores.ss")
	(require-relative-library "mysterxu.ss")
	(system "REGSVR32 /s compiled/native/myspage.dll")
	(system "REGSVR32 /s compiled/native/myssink.dll"))]
    [else (failure-thunk)]))
