(module check-gui mzscheme
  (require (lib "unitsig.ss"))

  (require "private/checksigs.ss")
  (require "private/gui-defs.ss")
  (require "private/cmdline.ss")
  (require "private/runcheck.ss")

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [DEFS : defs^ (gui-defs@)]
     [ARGS : args^ (cmdline@)]
     [RUNCHECK : empty^ (runcheck@ (ARGS) (DEFS))])
    (export))))

