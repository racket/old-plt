;;
;; $Id: link.ss,v 1.45 1997/12/04 21:28:32 mflatt Exp robby $
;;

(compound-unit/sig (import [core : mzlib:core^])
  (link [wx : wx^ (wx@)]
	[mred : mred^ ((reference-unit/sig "linkwx.ss") core wx)])
  (export (open mred)))
