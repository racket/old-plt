;;
;; $Id: testr.ss,v 1.1 1998/11/19 17:23:42 robby Exp $
;;
;; Link the gui tester together into compound unit.
;;
;; keymap (keys.ss) must be evaled before stprims.ss,
;; so mred:shifted-key-list is defined.
;;   

(compound-unit/sig

  (import [mred : mred-interfaces^]
	  [keymap : framework:keymap^])

  (link
    [run : framework:test:run^
      ((require-unit/sig "test-run.ss") mred)]
    [prim : framework:test:primitives^
      ((require-unit/sig "test-prims.ss") mred keymap run)])

  (export
    (open run)
    (open prim)))
