(module help-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "browser-unit.ss" "browser")
           "help-sig.ss"
           "private/sig.ss"
           "private/helpwin.ss"
           "private/search.ss"
           (lib "mred-sig.ss" "mred"))
  
  (provide help@)
  
  (define help@
    (compound-unit/sig
      (import [plt-installer : plt-installer^]
              [mred : mred^]
              [framework : framework^]
              [mixin : (frame-mixin)]
              [doc-position : help:doc-position^])
      (link [browser : browser^ (browser@ plt-installer mred)]
            [search : search^ (search@ doc-position function)]
            [helpwin : help-window^ (helpwin@ search browser plt-installer mred framework mixin)])
      (export (open helpwin)
              (var (search doc-collections-changed))))))
