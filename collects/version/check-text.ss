(module check-text mzscheme
  (require (lib "unitsig.ss"))

  (require "private/checksigs.ss")
  (require "private/text-defs.ss")
  (require "private/runcheck.ss")

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [DEFS : defs^ (text-defs@)]
     [RUNCHECK : empty^ (runcheck@ (DEFS))])
    (export))))


