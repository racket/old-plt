(compound-unit/sig (import [function : mzlib:function^]
			   [wx : wx^])
  (link [constants : mred:constants^ ((require-unit/sig "constant.ss"))]
	[testable : mred:testable-window^
		  ((require-unit/sig "testable.ss") wx)]
	[connections : mred:connections^
		     ((require-unit/sig "connect.ss")
		      wx
		      constants
		      function
		      testable)]
	[container : mred:container^
		   ((require-unit/sig "containr.ss") wx 
		    constants testable connections function)])
  (export (unit constants)
	  (unit testable)
	  (unit connections)
	  (unit container)))
