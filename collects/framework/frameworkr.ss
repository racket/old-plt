(compound-unit/sig
  (import [core : mzlib:core^]
	  [mred : mred^])
  (link
   [pref-file : framework:prefs-file^ ((require-relative-library "prefs-file.ss"))]
   [f : framework^ ((require-relative-library "frameworkp.ss")
		    core
		    mred
		    pref-file)])
  (export
   (open f)))
