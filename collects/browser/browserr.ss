
(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^))
 (link [HTML : browser:html^ ((require-relative-library "html.ss") 
			      FILE STRING URL MRED)]
       [HYPER : browser^ ((require-relative-library "hyper.ss") 
			  HTML FUNCTION STRING URL MRED)])
 (export (open HYPER)))
