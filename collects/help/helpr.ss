
(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^))
 (link [BROWSER : browser^ ((require-library "browserr.ss" "browser")
			    FUNCTION STRING FILE URL MRED)]
       [HELP : () ((require-relative-library "helpwin.ss")
		   BROWSER FUNCTION STRING FILE URL MRED)])
 (export))
