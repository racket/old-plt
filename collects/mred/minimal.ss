(compound-unit/sig (import [function : mzlib:function^]
			   [wx : mred:wx^])
  (link [constants : mred:constants^ ((reference-unit/sig "constant.ss"))]
	[testable : mred:testable-window^
		  ((reference-unit/sig "testable.ss") wx)]
	[connections : mred:connections^
		     ((reference-unit/sig "connect.ss")
		      wx
		      constants
		      function
		      testable)]
	[container : mred:container^
		   ((reference-unit/sig "containr.ss") wx 
		    constants testable connections function)])
  (export (unit constants)
	  (unit testable)
	  (unit connections)
	  (unit container)))
