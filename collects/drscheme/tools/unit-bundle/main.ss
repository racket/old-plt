;(current-library-collection-paths '("d:\\plt\\collects"))
;(current-library-collection-paths (list (build-path "Cupertino:" "robby" "plt" "collects")))
;(current-library-collection-paths (list "/home/robby/plt/collects"))

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
                  ((require-library "bundle-model.ss" "drscheme" "tools" "unit-bundle"))]
    [bundle-view/control : drscheme:bundle:bundle-view/control^
                         ((require-library "bundle-view-control.ss" "drscheme" "tools" "unit-bundle")
                          bundle-model mred misc)]
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
