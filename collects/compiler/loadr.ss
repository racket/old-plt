
;; A compound unit for Scheme->C compilation without MrSpidey
;;  analysis.

(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (PRETTY-PRINT : mzlib:pretty-print^)
	 (FILE : mzlib:file^)
	 (STRING : mzlib:string^)
	 (COMPILE : dynext:compile^)
	 (LINK : dynext:link^)
	 (DFILE : dynext:file^)
	 (OPTIONS : compiler:option^))
 (link
  [BASE : compiler:basic-link^ ((require-relative-library-unit/sig "baseloadr.ss")
				FUNCTION
				PRETTY-PRINT
				FILE
				STRING
				COMPILE
				LINK
				DFILE
				OPTIONS
				SPIDEY)]
  [SPIDEY : compiler:mrspidey^
	  ((require-relative-library-unit/sig "spnoop.ss"))])
 (export (open ((BASE DRIVER) : compiler:inner^))))
