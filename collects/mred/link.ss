;;
;; $Id: link.ss,v 1.46 1997/12/08 18:41:42 robby Exp robby $
;;

(compound-unit/sig (import [core : mzlib:core^])
  (link [wx : wx^ (wx@)]
	[mred : mred^ ((reference-unit/sig "linkwx.ss") core wx)])
  (export (open mred)))
