;;
;; $Id: frameworkr.ss,v 1.16 1999/02/02 22:28:27 robby Exp $
;;

(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred-interfaces^])
  (link [keys : framework:keys^ ((require-relative-library "keys.ss"))]
	[test : framework:test^ ((require-relative-library "testr.ss") mred keys)]
	[f : frameworkc^ ((require-relative-library "frameworkc.ss")
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
   (open f)))
