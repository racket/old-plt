
(compound-unit/sig
 (import (FUNCTION : mzlib:function^)
	 (STRING : mzlib:string^)
	 (FILE : mzlib:file^)
	 (URL : mzlib:url^)
	 (MRED : mred^))
 (link [BTREE : relative-btree^ ((require-relative-library "btree.ss"))]
       [BULLET : bullet-snip^ ((require-relative-library "bullet.ss") MRED)]
       [HTML : browser:html^ ((require-relative-library "html.ss") 
			      FILE STRING BTREE URL BULLET MRED)]
       [HYPER : browser^ ((require-relative-library "hyper.ss") 
			  HTML FUNCTION STRING URL MRED)])
 (export (open HYPER)))
