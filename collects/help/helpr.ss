
(compound-unit/sig
 (import (OPTION : help:option^)
	 (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^))
 (link [BROWSER : browser^ ((require-library "browserr.ss" "browser")
			    FUNCTION STRING FILE URL MRED)]
       [HELP : () ((require-relative-library "helpwin.ss")
		   OPTION BROWSER FUNCTION STRING FILE URL MRED)])
 (export))
