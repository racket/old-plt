(require-library "bundle-sig.ss" "drscheme" "tools" "unit-bundle")

(invoke-unit/sig
 (compound-unit/sig
   (import [mred : mred^])
   
   (link
    [core : mzlib:core^ ((require-library "corer.ss"))]
    [framework : framework^ ((require-library "frameworkr.ss" "framework") core mred)]
    [misc : drscheme:bundle:misc^
	  ((require-library "misc.ss" "drscheme" "tools" "unit-bundle") mred)]
    [bundle-model : drscheme:bundle:bundle-model^
                  ((require-library "bundle-model.ss" "drscheme" "tools" "unit-bundle")
                   mred bundle-view/control)]
    [bundle-view/control : drscheme:bundle:bundle-view/control^
                         ((require-library "bundle-view-control.ss"
                                           "drscheme" "tools" "unit-bundle")
                          mred misc bundle-model)]
    [compound-unit : drscheme:bundle:compound-unit^
		   ((require-library "compound-unit.ss" "drscheme" "tools" "unit-bundle")
		    mred
                    framework)]
    [main : ()
          ((unit/sig ()
             (import drscheme:bundle:bundle-view/control^
                     drscheme:bundle:compound-unit^)
             
             (new-bundle-table-frame))
           bundle-view/control
           compound-unit)])
   
   (export))
 mred^)
