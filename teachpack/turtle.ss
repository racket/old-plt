(require-library "value-turtles.ss" "graphics")

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link (TURTLE : value-turtles^
		((require-library "value-turtle-lib.ss" "graphics")
		 (PLT : mzlib:function^)
		 (PLT : mzlib:math^)
		 (PLT : mred^))))
  (export (open TURTLE)))
