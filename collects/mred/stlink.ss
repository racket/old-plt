;;
;; $Id: stlink.ss,v 1.13 1998/01/27 21:54:16 robby Exp $
;;
;; Link the gui tester together into compound unit.
;;
;; keymap (keys.ss) must be evaled before stprims.ss,
;; so mred:shifted-key-list is defined.
;;   

(compound-unit/sig

  (import 
    [wx       : wx^]
    [testable : mred:testable-window^]
    [keymap   : mred:keymap^])

  (link
    [run : mred:test:run^
      ((require-unit/sig "strun.ss") wx)]
    
    [prim : mred:test:primitives^
      ((require-unit/sig "stprims.ss") wx testable keymap run)]

    [drscheme : mred:test:drscheme^
      ((require-unit/sig "stdrs.ss") wx testable)])

  (export
    (open run)
    (open prim)
    (unit drscheme drs)))
