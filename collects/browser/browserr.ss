
(compound-unit/sig
 (import (function : mzlib:function^)
	 (string : mzlib:string^)
	 (file : mzlib:file^)
	 (url : mzlib:url^)
	 (mred : mred^)
	 (framework : framework^))
 (link [btree : relative-btree^ ((require-relative-library "btree.ss"))]
       [bullet : bullet-snip^ ((require-relative-library "bullet.ss") mred)]
       [html : browser:html^ ((require-relative-library "html.ss") 
			      file string btree url bullet mred)]
       [plt-installer : plt-installer^
		      ((require-library "plt-installerr.ss" "setup"))]
       [hyper : browser^ ((require-relative-library "hyper.ss") 
			  html function file string url bullet mred framework
			  plt-installer)])
 (export (open hyper)))
