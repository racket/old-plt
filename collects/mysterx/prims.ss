;; prims.ss

(unit->unit/sig 
 (load-extension 
  (build-path (collection-path "mysterx") 
	      "compiled" "native" "win32" "i386" "mxmain.dll"))
 ()
 mysterx:prims^)
