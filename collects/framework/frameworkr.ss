;;
;; $Id: frameworkr.ss,v 1.15 1998/11/19 21:24:36 robby Exp $
;;

(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred-interfaces^])
  (link [keys : framework:keys^ ((require-relative-library "keys.ss"))]
	[test : framework:test^ ((require-relative-library "testr.ss") mred keys)]
	[F : frameworkc^ ((require-relative-library "frameworkc.ss")
			  (core string)
			  (core function)
			  (core pretty-print)
			  (core file)
			  (core thread)
			  mred
			  keys
			  test)])
  (export
   (unit keys)
   (unit test)
   (open F)))
