

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
  [SPARAMS : plt:parameters^ ((reference-library-unit/sig "sparamr.ss" "backward"))]
  [ZODIAC : zodiac:system^ ((reference-library-unit/sig "link.ss" "zodiac")
			    (ZLAYER : zodiac:interface^)
			    SPARAMS
			    PRETTY-PRINT
			    FILE)]
  [ZLAYER : compiler:zlayer^ ((reference-relative-library-unit/sig "zlayer.ss")
			      OPTIONS
			      ZODIAC
			      CSTRUCTS
			      DRIVER
			      FUNCTION)]
  [INTERACTION : mrspidey:interaction^
	       ((reference-relative-library-unit/sig "mrspidey.ss")
		OPTIONS
		ZODIAC
		ZLAYER
		DRIVER)]
  [MRSPIDEY : mrspidey:sba^
	    ((reference-library-unit/sig "sba.ss" "mrspidey")
	     INTERACTION 
	     (FUNCTION : mrspidey:mzlib:function^)
	     PRETTY-PRINT 
	     FILE
	     STRING 
	     ZODIAC)]
  [LIBRARY : compiler:library^ ((reference-relative-library-unit/sig "library.ss")
				ZODIAC
				FUNCTION)]
  [CSTRUCTS : compiler:cstructs^ ((reference-relative-library-unit/sig "cstructs.ss")
				  LIBRARY
				  ZODIAC
				  ZLAYER
				  FUNCTION)]
  [PREPHASE : compiler:prephase^ ((reference-relative-library-unit/sig "prephase.ss")
				  OPTIONS
				  LIBRARY
				  CSTRUCTS
				  ZODIAC
				  ZLAYER
				  DRIVER)]
  [ANORM : compiler:anorm^ ((reference-relative-library-unit/sig "anorm.ss")
			    OPTIONS
			    LIBRARY
			    CSTRUCTS
			    ZODIAC
			    ZLAYER
			    DRIVER
			    FUNCTION)]
  [CONST : compiler:const^ ((reference-relative-library-unit/sig "const.ss")
			    OPTIONS
			    LIBRARY
			    CSTRUCTS
			    ZODIAC
			    ANALYZE
			    ZLAYER
			    VMSTRUCTS
			    TOP-LEVEL
			    DRIVER)]
  [ANALYZE : compiler:analyze^ ((reference-relative-library-unit/sig "analyze.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				PREPHASE
				ANORM
				CONST
				REP
				DRIVER
				FUNCTION
				MRSPIDEY)]
  [CLOSURE : compiler:closure^ ((reference-relative-library-unit/sig "closure.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				CONST
				DRIVER)]
  [VEHICLE : compiler:vehicle^ ((reference-relative-library-unit/sig "vehicle.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				CONST
				ANALYZE
				CLOSURE
				DRIVER)]
  [REP : compiler:rep^ ((reference-relative-library-unit/sig "rep.ss")
			LIBRARY
			CSTRUCTS
			ZODIAC
			ZLAYER
			CONST
			VEHICLE
			DRIVER)]
  [VMSTRUCTS : compiler:vmstructs^ ((reference-relative-library-unit/sig "vmscheme.ss")
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    DRIVER
				    FUNCTION)]
  [VMPHASE : compiler:vmphase^ ((reference-relative-library-unit/sig "vmphase.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				CONST
				VMSTRUCTS
				REP
				VEHICLE
				DRIVER
				FUNCTION)]
  [VMOPT : compiler:vmopt^ ((reference-relative-library-unit/sig "vmopt.ss")
			    OPTIONS
			    LIBRARY
			    CSTRUCTS
			    ZODIAC
			    ZLAYER
			    VMSTRUCTS
			    ANALYZE
			    REP
			    VMPHASE
			    DRIVER
			    FUNCTION)]
  [VM2C : compiler:vm2c^ ((reference-relative-library-unit/sig "vm2c.ss")
			  OPTIONS
			  LIBRARY
			  CSTRUCTS
			  ZODIAC
			  ZLAYER
			  ANALYZE
			  CONST
			  REP
			  VEHICLE
			  VMSTRUCTS
			  DRIVER)]
  [TOP-LEVEL : compiler:top-level^ ((reference-relative-library-unit/sig "toplevel.ss"))]
  [DRIVER : compiler:driver^ ((reference-relative-library-unit/sig "driver.ss")
			      OPTIONS
			      LIBRARY
			      CSTRUCTS
			      ZODIAC
			      ZLAYER
			      PREPHASE
			      ANORM
			      ANALYZE
			      CONST
			      CLOSURE
			      VEHICLE
			      REP
			      VMSTRUCTS
			      VMPHASE
			      VMOPT
			      VM2C
			      TOP-LEVEL
			      COMPILE
			      LINK
			      DFILE
			      FUNCTION
			      PRETTY-PRINT
			      MRSPIDEY)])
 (export (open (DRIVER : compiler:inner^))))

