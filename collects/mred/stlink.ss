;;
;; $Id: stlink.ss,v 1.10 1997/08/15 22:36:07 krentel Exp mflatt $
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
    [run : mred:test:run^
      ((reference-unit/sig "strun.ss") wx)]
    
    [prim : mred:test:primitives^
      ((reference-unit/sig "stprims.ss") wx testable keymap run)]

    [drscheme : mred:test:drscheme^
      ((reference-unit/sig "stdrs.ss") wx testable)])

  (export
    (open run)
    (open prim)
    (unit drscheme drs))

  )
