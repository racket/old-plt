(module check-gui mzscheme
  (require (lib "unitsig.ss"))

  (require "private/checksigs.ss")
  (require "private/gui-defs.ss")
  (require "private/runcheck.ss")

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [DEFS : defs^ (gui-defs@)]
     [RUNCHECK : empty^ (runcheck@ (DEFS))])
    (export))))

