
;; A compound unit for Scheme->C compilation using MrSpidey
;;  analysis. Not yet ported to v200.

;; See also nospidey-unit.ss

#|

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
  [INTERACTION : mrspidey:interaction^
	       ((require-relative-library-unit/sig "mrspideyi.ss")
		OPTIONS
		(BASE ZODIAC)
		(BASE ZLAYER)
		(BASE DRIVER))]
  [FILE-READ : mrspidey:file-read^
	     ((require-relative-library-unit/sig "mrspideyf.ss"))]
  [MRSPIDEY : mrspidey:sba^
	    ((require-library-unit/sig "sba.ss" "mrspidey")
	     INTERACTION
	     FILE-READ
	     (FUNCTION : mrspidey:mzlib:function^)
	     PRETTY-PRINT 
	     FILE
	     STRING 
	     (BASE ZODIAC))]
  ;; The glue SPIDEY must be initialized after the real MRSPIDEY
  [SPIDEY : compiler:mrspidey^
	  ((require-relative-library-unit/sig "mrspidey.ss")
	   MRSPIDEY
	   (BASE LIBRARY))])
 (export (open ((BASE DRIVER) : compiler:inner^))))

|#
