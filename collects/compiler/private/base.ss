
(module base mzscheme
  (require (lib "unitsig.ss"))

  (require "../sig.ss")
  (require "sig.ss")

  (require (lib "zodiac-sig.ss" "syntax")
	  (lib "zodiac-unit.ss" "syntax"))

  (require (lib "file-sig.ss" "dynext")
	  (lib "link-sig.ss" "dynext")
	  (lib "compile-sig.ss" "dynext"))

  (require "zlayer.ss"
	  "library.ss"
	  "cstructs.ss"
	  "prephase.ss"
	  "anorm.ss"
	  "const.ss"
	  "known.ss"
	  "analyze.ss"
	  "lift.ss"
	  "closure.ss"
	  "vehicle.ss"
	  "rep.ss"
	  "vmscheme.ss"
	  "vmphase.ss"
	  "vmopt.ss"
	  "vm2c.ss"
	  "lightweight.ss"
	  "toplevel.ss"
	  "driver.ss")

  ;; The core Scheme->C compiler linkage, including everything
  ;;  that's common to MrSpidey and non-MrSpidey compilation.

  (provide base@)

  (define base@
    (compound-unit/sig
     (import (COMPILE : dynext:compile^)
	     (LINK : dynext:link^)
	     (DFILE : dynext:file^)
	     (OPTIONS : compiler:option^)
	     (SPIDEY : compiler:mrspidey^))
     (link
      [ZODIAC : zodiac^ (zodiac@)]
      [ZLAYER : compiler:zlayer^ (zlayer@
				  OPTIONS
				  ZODIAC
				  CSTRUCTS
				  DRIVER
				  SPIDEY)]
      [LIBRARY : compiler:library^ (library@
				    ZODIAC)]
      [CSTRUCTS : compiler:cstructs^ (cstructs@
				      LIBRARY
				      ZODIAC
				      ZLAYER)]
      [PREPHASE : compiler:prephase^ (prephase@
				      OPTIONS
				      LIBRARY
				      CSTRUCTS
				      ZODIAC
				      ZLAYER
				      DRIVER
				      SPIDEY)]
      [ANORM : compiler:anorm^ (anorm@
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				DRIVER
				SPIDEY)]
      [CONST : compiler:const^ (const@
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ANALYZE
				ZLAYER
				VMSTRUCTS
				TOP-LEVEL
				DRIVER)]
      [KNOWN : compiler:known^ (known@
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				PREPHASE
				ANORM
				CONST
				CLOSURE
				REP
				DRIVER
				SPIDEY)]
      [LIGHTWEIGHT : compiler:lightweight^ (lightweight@
					    OPTIONS
					    LIBRARY
					    CSTRUCTS
					    ZLAYER
					    CONST
					    TOP-LEVEL
					    DRIVER
					    ZODIAC)]
      [ANALYZE : compiler:analyze^ (analyze@
				    OPTIONS
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    PREPHASE
				    ANORM
				    KNOWN
				    CONST
				    REP
				    VM2C
				    DRIVER
				    SPIDEY)]
      [LIFT : compiler:lift^ (lift@
			      OPTIONS
			      LIBRARY
			      CSTRUCTS
			      ZODIAC
			      ZLAYER
			      KNOWN
			      TOP-LEVEL
			      ANALYZE
			      CONST
			      CLOSURE
			      DRIVER)]
      [CLOSURE : compiler:closure^ (closure@
				    OPTIONS
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    CONST
				    DRIVER)]
      [VEHICLE : compiler:vehicle^ (vehicle@
				    OPTIONS
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    CONST
				    KNOWN
				    CLOSURE
				    DRIVER)]
      [REP : compiler:rep^ (rep@
			    LIBRARY
			    CSTRUCTS
			    ANALYZE
			    ZODIAC
			    ZLAYER
			    CONST
			    VEHICLE
			    DRIVER)]
      [VMSTRUCTS : compiler:vmstructs^ (vmscheme@
					LIBRARY
					CSTRUCTS
					ZODIAC
					ZLAYER
					DRIVER)]
      [VMPHASE : compiler:vmphase^ (vmphase@
				    OPTIONS
				    LIBRARY
				    CSTRUCTS
				    ZODIAC
				    ZLAYER
				    ANALYZE
				    CONST
				    VMSTRUCTS
				    REP
				    CLOSURE
				    VEHICLE
				    DRIVER)]
      [VMOPT : compiler:vmopt^ (vmopt@
				OPTIONS
				LIBRARY
				CSTRUCTS
				ZODIAC
				ZLAYER
				VMSTRUCTS
				KNOWN
				REP
				VMPHASE
				DRIVER)]
      [VM2C : compiler:vm2c^ (vm2c@
			      OPTIONS
			      LIBRARY
			      CSTRUCTS
			      ZODIAC
			      ZLAYER
			      ANALYZE
			      CONST
			      REP
			      CLOSURE
			      VEHICLE
			      VMSTRUCTS
			      DRIVER)]
      [TOP-LEVEL : compiler:top-level^ (toplevel@
					LIBRARY
					CSTRUCTS)]
      [DRIVER : compiler:driver^ (driver@
				  OPTIONS
				  LIBRARY
				  CSTRUCTS
				  ZODIAC
				  ZLAYER
				  PREPHASE
				  ANORM
				  KNOWN
				  ANALYZE
				  CONST
				  LIFT
				  LIGHTWEIGHT
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
				  SPIDEY)])
     (export (unit ZODIAC)
	     (unit ZLAYER)
	     (unit DRIVER)
	     (unit LIBRARY)))))


