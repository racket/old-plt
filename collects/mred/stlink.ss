;;
;; $Id$
;;
;; Link the gui tester together into compound unit.
;;

(compound-unit/sig

  (import 
    [wx : mred:wx^])

  (link
    [global : mred:test:global^
      ((reference-unit/sig "stglobal.ss"))]
   
    [frame : mred:test:active-frame^
      ((reference-unit/sig "stframe.ss") wx)]

    [run : mred:test:run^
      ((reference-unit/sig "strun.ss") global)]

    [prim : mred:test:primitives^
      ((reference-unit/sig "stprims.ss") wx global frame)])

  (export
    (open global)
    (open frame)
    (open run)
    (open prim))
  )
