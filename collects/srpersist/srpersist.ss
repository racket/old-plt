;; srpersist.ss

(unless (defined? 'odbc-version)
	(error "odbc-version not defined"))

(current-require-relative-collection '("srpersist"))

(require-library "macro.ss")
(require-library "cores.ss")
(require-relative-library "srpersistu.ss")

(cond

 [(>= odbc-version 3.5)
  (require-relative-library "invoke-3.5.ss")]

 [(>= odbc-version 3.0)
  (require-relative-library "invoke-3.0.ss")]

 [(>= odbc-version 2.0)
  (require-relative-library "invoke-2.0.ss")]

 [(>= odbc-version 1.0)
  (require-relative-library "invoke-1.0.ss")])

