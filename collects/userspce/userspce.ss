; require this file to make mzscheme as much like drscheme as possible.

(read-case-sensitive #t)
(require-library "refer.ss")
(require-library "userspcs.ss" "userspce")

(invoke-open-unit/sig
 (compound-unit/sig (import)
   (link [P : plt:userspace:params^ ((require-relative-library "paramr.ss"))]
	 [U : plt:userspace^ ((require-relative-library "userspcr.ss") P)])
   (export (open P)
	   (open U))))

