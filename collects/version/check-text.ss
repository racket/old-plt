(module check-text mzscheme
  (require (lib "unitsig.ss"))

  (require "private/checksigs.ss")
  (require "private/text-defs.ss")
  (require "private/cmdline.ss")
  (require "private/runcheck.ss")

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [DEFS : defs^ (text-defs@)]
     [PROGNAME : progname^ (text-defs@)]
     [ARGS : args^ (cmdline@ (PROGNAME))]
     [RUNCHECK : empty^ (runcheck@ (DEFS) (ARGS))])
    (export))))






