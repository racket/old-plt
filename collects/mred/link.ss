;;
;; $Id: link.ss,v 1.47 1998/01/27 21:54:14 robby Exp $
;;

(compound-unit/sig (import [core : mzlib:core^])
  (link [wx : wx^ (wx@)]
	[mred : mred^ ((require-unit/sig "linkwx.ss") core wx)])
  (export (open mred)))
