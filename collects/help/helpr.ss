
(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^)
	 (FRAMEWORK : framework^))
 (link [BROWSER : browser^ ((require-library "browserr.ss" "browser")
			    FUNCTION STRING FILE URL MRED)]
       [SEARCH : help:search^ ((require-relative-library "search.ss")
			       FUNCTION)]
       [HELP : help:help^
	     ((require-relative-library "helpwin.ss")
	      SEARCH
	      BROWSER FUNCTION STRING FILE URL
	      MRED FRAMEWORK)])
 (export (open HELP)))

