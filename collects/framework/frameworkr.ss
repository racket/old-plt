;;
;; $Id: frameworkr.ss,v 1.17 1999/03/16 04:51:40 robby Exp $
;;

(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred^])
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
