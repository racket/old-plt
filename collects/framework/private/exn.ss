(module exn mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide exn@)

  (define exn@
    (unit/sig framework:exn^
      (import)

      (rename [struct:-exn struct:exn]
	      [make--exn make-exn]
	      [-exn? exn?])

      (define-struct (-exn struct:exn) ())
      (define-struct (unknown-preference struct:exn) ())
      (define-struct (during-preferences struct:exn) ()))))
