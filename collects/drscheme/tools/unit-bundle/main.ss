(current-library-collection-paths '("d:\\plt\\collects"))

(require-library "bundle-sig.ss" "drscheme" "tools" "unit-bundle")

(invoke-unit/sig
 (compound-unit/sig
   (import [mred : mred^])
   (link
    [misc : drscheme:bundle:misc^
	  ((require-library "misc.ss" "drscheme" "tools" "unit-bundle") mred)]
    [bundle : drscheme:bundle:bundle^
	    ((require-library "bundle.ss" "drscheme" "tools" "unit-bundle") mred misc)]
    [compound-unit : drscheme:bundle:compound-unit^
		   ((require-library "compound-unit.ss" "drscheme" "tools" "unit-bundle")
		    mred)])
   (export))
 mred^)
