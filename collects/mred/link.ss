;;
;; $Id: link.ss,v 1.43 1997/10/09 21:42:24 robby Exp robby $
;;

(compound-unit/sig (import [core : mzlib:core^])
  (link [wx : mred:wx^ ((unit/sig () (import)))]
	[mred : mred^ ((reference-unit/sig "linkwx.ss") core wx)])
  (export (open mred)))
