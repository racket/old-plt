
(compound-unit/sig
  (import (function : mzlib:function^)
	  (string : mzlib:string^)
	  (file : mzlib:file^)
	  (url : mzlib:url^)
	  (mred : mred^)
	  (framework : framework^)
	  (mixin : (frame-mixin))
	  [doc-position : help:doc-position^])
  (link [browser : browser^ ((require-library "browserr.ss" "browser")
			     function string file url mred framework)]
	[search : help:search^ ((require-relative-library "search.ss")
				doc-position function)]
	[info : setup:info^ ((require-library "get-infor.ss" "setup"))]
	[help : help:help-window^
	      ((require-relative-library "helpwin.ss")
	       info search
	       browser function string file url
	       mred framework mixin)])
  (export (open help)
	  (var (search doc-collections-changed))))
