(load-relative "sig.ss")
(load-relative "jvm.ss")

(invoke-unit/sig
  (compound-unit/sig
    (import)
    (link
     (DRJ : gui^ ((load-relative "cu+frame.ss")))
     (MAIN : () ((unit/sig ()
                   (import gui^)
                   (new-document #f))
                 DRJ)))
    (export)))
