(module version mzscheme

  (provide check-version)

  (require (lib "unitsig.ss"))

  (require "private/checksigs.ss")
  (require "private/gui-defs.ss")
  (require "private/runcheck.ss")

  (define args@
    (unit/sig args^
      (import progname^)
      (define collections '())))

  (define (check-version)
    (invoke-unit/sig
     (compound-unit/sig
      (import)
      (link
       [DEFS : defs^ (gui-defs@)]
       [PROGNAME : progname^ (gui-defs@)]
       [ARGS : args^ (args@ (PROGNAME))]
       [RUNCHECK : empty^ (runcheck@ (DEFS) (ARGS))])
      (export)))))








