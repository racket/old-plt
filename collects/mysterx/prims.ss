;; prims.ss

(unit->unit/sig 
 (load-relative-extension "compiled/native/win32/i386/mysterx.dll")
 ()
 mysterx:prims^)
