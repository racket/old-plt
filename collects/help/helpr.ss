
(compound-unit/sig
 (import (OPTION : help:option^)
	 (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^))
 (link [BROWSER : browser^ ((require-library "browserr.ss" "browser")
			    FUNCTION STRING FILE URL MRED)]
       [SEARCH : help:search^ ((require-relative-library "search.ss")
			       HELP)]
       [HELP : help:help^
	     ((require-relative-library "helpwin.ss")
	      SEARCH
	      OPTION BROWSER FUNCTION STRING FILE URL MRED)])
 (export))
