;;
;; $Id: stlink.ss,v 1.8 1997/08/13 15:34:41 krentel Exp krentel $
;;
;; Link the gui tester together into compound unit.
;;
;; keymap (keys.ss) must be evaled before stprims.ss,
;; so mred:shifted-key-list is defined.
;;   

(compound-unit/sig

  (import 
    [wx       : mred:wx^]
    [testable : mred:testable-window^]
    [keymap   : mred:keymap^])

  (link
    [global : mred:test:globals^
      ((reference-unit/sig "stglobal.ss") wx testable)]

    [run : mred:test:run^
      ((reference-unit/sig "strun.ss"))]
    
    [prim : mred:test:primitives^
      ((reference-unit/sig "stprims.ss") wx testable keymap global run)]

    [drscheme : mred:test:drscheme^
      ((reference-unit/sig "stdrs.ss") wx global)])

  (export
    (open run)
    (open prim)
    (unit drscheme drs))

  )
