;;
;; $Id: stlink.ss,v 1.15 1998/05/12 21:52:53 steck Exp $
;;
;; Link the gui tester together into compound unit.
;;
;; keymap (keys.ss) must be evaled before stprims.ss,
;; so mred:shifted-key-list is defined.
;;   

(compound-unit/sig

  (import [mred : mred^]
	  [keymap : framework:keymap^])

  (link
    [run : mred:test:run^
      ((require-unit/sig "test-run.ss") mred)]
    
    [prim : mred:test:primitives^
      ((require-unit/sig "test-prims.ss") keymap run)])

  (export
    (open run)
    (open prim)))
