(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred^]
	  [pref-file : framework:prefs-file^])
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
			  test
			  pref-file)])
  (export
   (unit keys)
   (unit test)
   (open f)))
