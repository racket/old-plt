;; link.ss
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------

(compound-unit/sig ; mrspidey:sba^
 (import
  (INTERACTION : mrspidey:interaction^)
  (FUNCTION  : mrspidey:mzlib:function^)
  (PRETTY-PRINT : mzlib:pretty-print^)
  (FILE : mzlib:file^)
  (STRING : mzlib:string^)
  (ZODIAC : zodiac:system^))
 (link
  [ZAUX : mrspidey:zodiac^ 
	((require-relative-library-unit/sig "zod-aux.ss")
	 CDL ZODIAC)]
  [CDL : mrspidey:CDL^ 
       ((compound-unit/sig
	 (import 
	  (I : mrspidey:interaction^)
	  (FUNCTION  : mrspidey:mzlib:function^)
	  (PRETTY-PRINT : mzlib:pretty-print^)
	  (FILE : mzlib:file^)
	  (STRING : mzlib:string^))
	 (link
	  [L : mrspidey:library^
	     ((require-relative-library-unit/sig "main.ss" "lib") 
	      I FUNCTION PRETTY-PRINT FILE STRING)]
	  [C : mrspidey:config^
	     ((require-relative-library-unit/sig "config.ss") 
	      L FUNCTION PRETTY-PRINT FILE STRING)]
	  [D : mrspidey:debugging^
	     ((require-relative-library-unit/sig "debug.ss")
	      L FUNCTION PRETTY-PRINT FILE STRING)])
	 (export (open C) (open D) (open L)))
	INTERACTION FUNCTION PRETTY-PRINT FILE STRING)]
  [LOADEXPAND : mrspidey:loadexpand^
	      ((require-relative-library-unit/sig "ldexpand.ss")
	       CDL INTERACTION LANGUAGES ZODIAC ZAUX 
	       FUNCTION PRETTY-PRINT FILE STRING)]
  [HASH : mrspidey:hash^
	((require-relative-library-unit/sig "hash.ss")
	 CDL INTERACTION FUNCTION PRETTY-PRINT FILE STRING)]
  [KERNEL : mrspidey:kernel^ 
	  ((require-relative-library-unit/sig "kernel.ss")
	   CDL HASH KERNEL-AUX TEMPLATES)]
  [MIN : mrspidey:min^
       ((compound-unit/sig
	 (import
	  (CDL : mrspidey:CDL^)
	  (INTERACTION : mrspidey:interaction^)
	  (KERNEL : mrspidey:kernel^)
	  (KERNEL-AUX : mrspidey:kernel-aux^)
	  (TEMPLATES : mrspidey:templates^)
	  (FUNCTION  : mrspidey:mzlib:function^)
	  (PRETTY-PRINT : mzlib:pretty-print^)
	  (FILE : mzlib:file^)
	  (STRING : mzlib:string^))
	 (link
	  [MIN : mrspidey:min^ 
	       ((require-relative-library-unit/sig "min.ss" "min")
		CDL INTERACTION KERNEL 
		FIND LIVE FEW FAST)]
	  [FIND : mrspidey:find-nonempty-tvars^
		((require-relative-library-unit/sig "nonempty.ss" "min")
		 CDL KERNEL KERNEL-AUX LIVE TEMPLATES)]
	  [LIVE : mrspidey:min-live^
		((require-relative-library-unit/sig "min-live.ss" "min")
		 CDL KERNEL KERNEL-AUX MIN TEMPLATES)]
	  [FEW : mrspidey:min-live-few-e^
	       ((require-relative-library-unit/sig "livefewe.ss" "min")
		CDL KERNEL KERNEL-AUX MIN LIVE)]
	  [HOPCROFT : mrspidey:hopcroft^
		    ((require-relative-library-unit/sig "hopcroft.ss" "min")
		     CDL FUNCTION PRETTY-PRINT FILE STRING MIN)]
	  [FAST : mrspidey:min-dfa-fast^
		((require-relative-library-unit/sig "min-dfa.ss" "min")
		 CDL KERNEL KERNEL-AUX MIN LIVE FEW HOPCROFT)])
	 (export (open MIN)))
	CDL INTERACTION KERNEL KERNEL-AUX 
	TEMPLATES FUNCTION PRETTY-PRINT FILE STRING)]
  [TEMPLATES : mrspidey:templates^
	     ((require-relative-library-unit/sig "template.ss")
	      CDL INTERACTION KERNEL)]
  [KERNEL-AUX : mrspidey:kernel-aux^
	      ((require-relative-library-unit/sig "kern-aux.ss")
	       CDL INTERACTION KERNEL TEMPLATES ZODIAC ZAUX)]
  [ATYPE : mrspidey:atype^
	 ((require-relative-library-unit/sig "atype.ss")
	  CDL INTERACTION KERNEL KERNEL-AUX
	  MIN TYPELANG TEMPLATES
	  TYPE-ENV SDL ATLUNIT FUNCTION PRETTY-PRINT FILE STRING)]
  [TYPELANG :
	    mrspidey:typelang^
	    ((require-relative-library-unit/sig "typelang.ss")
	     CDL INTERACTION KERNEL KERNEL-AUX
	     TEMPLATES TYPE-ENV ATYPE CONTAINED 
	     FUNCTION PRETTY-PRINT FILE STRING ZODIAC ZAUX)]
  [CONTAINED : mrspidey:contained^
	     ((require-relative-library-unit/sig "contain.ss")
	      CDL KERNEL SDL)]
  [TYPE-ENV : mrspidey:type-env^
	    ((require-relative-library-unit/sig "type-env.ss")
	     CDL INTERACTION KERNEL KERNEL-AUX
	     TEMPLATES MIN ATYPE ZODIAC ZAUX)]
  [SDL : mrspidey:sdl^ 
       ((require-relative-library-unit/sig "sdl.ss")
	CDL KERNEL MIN TYPELANG KERNEL-AUX TEMPLATES ATYPE
	FUNCTION PRETTY-PRINT FILE STRING)]
  [LANGUAGES : mrspidey:languages^
	     ((require-relative-library-unit/sig "language.ss")
	      CDL INTERACTION KERNEL
	      TEMPLATES KERNEL-AUX 
	      ATYPE TYPE-ENV TYPELANG ATENV LOADEXPAND TRAVERSE
	      FUNCTION PRETTY-PRINT FILE STRING ZODIAC ZAUX)]
  [ATENV : mrspidey:atenv^
	 ((require-relative-library-unit/sig "atenv.ss")
	  CDL INTERACTION KERNEL KERNEL-AUX ATYPE
	  ZODIAC ZAUX FUNCTION PRETTY-PRINT FILE STRING)]
  [TRAVERSE : mrspidey:traverse^
	    ((require-relative-library-unit/sig "traverse.ss")
	     CDL INTERACTION KERNEL MIN
	     LOADEXPAND
	     TEMPLATES KERNEL-AUX TYPELANG TYPE-ENV
	     LANGUAGES ATYPE ATLUNIT ATENV
	     ZODIAC ZAUX FUNCTION PRETTY-PRINT FILE STRING)]
  [ATLUNIT : mrspidey:atlunit^
	   ((require-relative-library-unit/sig "atlunit.ss")
	    CDL INTERACTION KERNEL KERNEL-AUX
	    MIN LOADEXPAND TYPE-ENV
	    TEMPLATES LANGUAGES ATYPE ATENV TRAVERSE
	    ZA ZODIAC ZAUX FUNCTION PRETTY-PRINT FILE STRING)]
  [ZA : mrspidey:za^ 
      ((require-relative-library-unit/sig "za.ss")
       CDL INTERACTION KERNEL TEMPLATES 
       TYPE-ENV TYPELANG ATYPE FUNCTION PRETTY-PRINT FILE STRING)]
  [PROGRAM : mrspidey:program^
	   ((require-relative-library-unit/sig "program.ss")
	    CDL INTERACTION 
	    KERNEL KERNEL-AUX TYPE-ENV
	    LOADEXPAND TRAVERSE
	    TEMPLATES ATENV ATYPE LANGUAGES
	    ZODIAC ZAUX FUNCTION PRETTY-PRINT FILE STRING)]
  [CHECKS : mrspidey:calc-checks^
	  ((require-relative-library-unit/sig "checks.ss")
	   CDL INTERACTION LOADEXPAND 
	   KERNEL KERNEL-AUX
	   TYPELANG TEMPLATES ATYPE SDL
	   ZODIAC ZAUX FUNCTION PRETTY-PRINT FILE STRING)]
  [DRIVER : mrspidey:driver^
	  ((require-relative-library-unit/sig "driver.ss")
	   CDL INTERACTION KERNEL
	   TEMPLATES TYPELANG TYPE-ENV ATYPE ATENV
	   PROGRAM CHECKS LANGUAGES ZODIAC ZAUX)]
  [HYPER : mrspidey:hyper^
	 ((require-relative-library-unit/sig "hyper.ss")
	  CDL INTERACTION LOADEXPAND
	  PROGRAM CHECKS TEMPLATES KERNEL KERNEL-AUX ATYPE 
	  FUNCTION PRETTY-PRINT FILE STRING)])
 (export 
  (open DRIVER)
  (open CDL)
  (open ATYPE)
  (open HYPER)
  (open KERNEL)
  (open CHECKS)
  (open LANGUAGES)
  (open ZAUX)
  (open PROGRAM)))



