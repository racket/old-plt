
(plt:require-library "ricedefu.ss")
(plt:require-library "sparamu.ss")

(invoke-open-unit/sig
 (compound-unit/sig
   (import)
   (link [P : plt:parameters^ (plt:mzscheme-parameters@)]
	 [R : ricedefs^ (ricedefs@ P)])
   (export (open R)))
 #f)
	 
