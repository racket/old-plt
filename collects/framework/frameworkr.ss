;;
;; $Id: frameworkr.ss,v 1.14 1998/11/19 20:53:19 robby Exp $
;;

(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred-interfaces^])
  (link [keys : framework:keys^ ((require-relative-library-unit/sig "keys.ss"))]
	[test : framework:test^ ((require-relative-library-unit/sig "testr.ss") mred keys)]
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
