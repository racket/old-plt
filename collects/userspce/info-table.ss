([name "Userspace"]
 [compile-prefix 
  (begin
    (require-library "refer.ss")
    (require-library "coreflats.ss")
    (require-library "errors.ss" "userspce")
    (require-library "params.ss" "userspce")
    (require-library "sig.ss" "userspce"))]
 [compile-omit-files ("sig.ss" "errors.ss" "params.ss" "ricedefs.ss"
	              "init-namespacer.ss"
		      "launcher-bootstrap.ss"
		      "launcher-bootstrap-mred.ss"
		      "launcher-bootstrap-mzscheme.ss")]
 [compile-elaboration-zos ("sig.ss")])
