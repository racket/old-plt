; require this file to make mzscheme as much like drscheme as possible.

(require-library "refer.ss")
(require-library "userspcs.ss" "userspce")

(invoke-open-unit/sig
 (compound-unit/sig (import)
   (link [p : plt:userspace:params^ ((require-relative-library "paramr.ss"))]
	 [u : plt:userspace^ ((require-relative-library "userspcr.ss") p)])
   (export (open p)
	   (open u))))

