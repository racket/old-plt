(module help-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "plt-installer-sig.ss" "setup")
           (lib "framework-sig.ss" "framework")
           (lib "browser-sig.ss" "browser")
           "help-sig.ss"
           "private/sig.ss"

           (lib "browser-unit.ss" "browser")
           "private/helpwin.ss"
           "private/search.ss"
           (lib "mred-sig.ss" "mred"))
  
  (provide help@)
  
  (define help@
    (compound-unit/sig
      (import [plt-installer : setup:plt-installer^]
              [mred : mred^]
              [framework : framework^]
              [mixin : (frame-mixin)]
              [doc-position : help:doc-position^])
      (link [hack : () ((unit/sig ()
                          (import (frame-mixin))
                          ;; communicate the frame-mixin to the bug report window
                          (namespace-variable-binding 'help-desk:frame-mixin frame-mixin))
                        mixin)]
            [browser : browser^ (browser@ plt-installer mred)]
            [search : search^ (search@ doc-position)]
            [helpwin : help-window^ (helpwin@ search browser plt-installer mred framework mixin)])
      (export (open helpwin)
              (var (search doc-collections-changed))))))
