
(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (PRETTY-PRINT : mzlib:pretty-print^)
	 (FILE : mzlib:file^)
	 (STRING : mzlib:string^)
	 (COMPILE : dynext:compile^)
	 (LINK : dynext:link^)
	 (DFILE : dynext:file^)
	 (OPTIONS : compiler:option^)
	 (SPIDEY : compiler:mrspidey^))
 (link
  [SPARAMS : plt:parameters^ ((require-library-unit/sig "sparamr.ss" "backward"))]
  [ZODIAC : zodiac:system^ ((require-library-unit/sig "link.ss" "zodiac")
			    (ZLAYER : zodiac:interface^)
			    SPARAMS
			    PRETTY-PRINT
			    FILE)]
  [ZLAYER : compiler:zlayer^ ((require-relative-library-unit/sig "zlayer.ss")
			      OPTIONS
			      ZODIAC
			      CSTRUCTS
			      DRIVER
			      FUNCTION
			      SPIDEY)]
  [LIBRARY : compiler:library^ ((require-relative-library-unit/sig "library.ss")
				ZODIAC
				FUNCTION)]
  [CSTRUCTS : compiler:cstructs^ ((require-relative-library-unit/sig "cstructs.ss")
				  LIBRARY
				  ZODIAC
				  ZLAYER
				  FUNCTION)]
  [PREPHASE : compiler:prephase^ ((require-relative-library-unit/sig "prephase.ss")
				  OPTIONS
				  LIBRARY
				  CSTRUCTS
				  ZODIAC
				  ZLAYER
				  DRIVER
				  SPIDEY)]
  [ANORM : compiler:anorm^ ((require-relative-library-unit/sig "anorm.ss")
			    OPTIONS
			    LIBRARY
			    CSTRUCTS
			    ZODIAC
			    ZLAYER
			    DRIVER
			    FUNCTION
			    SPIDEY)]
  [CONST : compiler:const^ ((require-relative-library-unit/sig "const.ss")
			    OPTIONS
			    LIBRARY
			    CSTRUCTS
			    ZODIAC
			    ANALYZE
			    ZLAYER
			    VMSTRUCTS
			    TOP-LEVEL
			    DRIVER)]
  [ANALYZE : compiler:analyze^ ((require-relative-library-unit/sig "analyze.ss")
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
				SPIDEY)]
  [CLOSURE : compiler:closure^ ((require-relative-library-unit/sig "closure.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				CONST
				DRIVER)]
  [VEHICLE : compiler:vehicle^ ((require-relative-library-unit/sig "vehicle.ss")
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				CONST
				ANALYZE
				CLOSURE
				DRIVER)]
  [REP : compiler:rep^ ((require-relative-library-unit/sig "rep.ss")
			LIBRARY
			CSTRUCTS
			ZODIAC
			ZLAYER
			CONST
			VEHICLE
			DRIVER)]
  [VMSTRUCTS : compiler:vmstructs^ ((require-relative-library-unit/sig "vmscheme.ss")
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    DRIVER
				    FUNCTION)]
  [VMPHASE : compiler:vmphase^ ((require-relative-library-unit/sig "vmphase.ss")
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
  [VMOPT : compiler:vmopt^ ((require-relative-library-unit/sig "vmopt.ss")
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
  [VM2C : compiler:vm2c^ ((require-relative-library-unit/sig "vm2c.ss")
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
  [TOP-LEVEL : compiler:top-level^ ((require-relative-library-unit/sig "toplevel.ss"))]
  [DRIVER : compiler:driver^ ((require-relative-library-unit/sig "driver.ss")
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
			      SPIDEY)])
 (export (unit ZODIAC)
	 (unit ZLAYER)
	 (unit DRIVER)))


