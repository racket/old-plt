;;
;; $Id: stlink.ss,v 1.2 1997/07/06 20:20:55 krentel Exp krentel $
;;
;; Link the gui tester together into compound unit.
;;

(compound-unit/sig

  (import 
    [wx : mred:wx^]
    [frame : mred:test:active-frame^])

  (link
    [struct : mred:test:struct^
      ((unit/sig mred:test:struct^
	(import)
	(define-struct event (thunk))))]
	    
    [global : mred:test:globals^
      ((reference-unit/sig "stglobal.ss") wx frame struct)]

    [run : mred:test:run^
      ((reference-unit/sig "strun.ss") struct global)]
    
    [prim : mred:test:primitives^
      ((reference-unit/sig "stprims.ss") wx struct global)]

    [drscheme : mred:test:drscheme^
      ((reference-unit/sig "stdrs.ss") wx struct global)])

  (export
    (open struct)
    (open run)
    (open prim)
    (open drscheme))
  
  )
