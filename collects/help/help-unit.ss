(module help-unit mzscheme
  (require (lib "unitsig.ss"))
  
  (provide help@)
  
  (define help@
    (compound-unit/sig
      (import [info : setup:info^]
              (mred : mred^)
              (framework : framework^)
              (mixin : (frame-mixin))
              [doc-position : help:doc-position^])
      (link [browser : browser^ ((require-library "browserr.ss" "browser")
                                 function string file url plt-installer
                                 mred)]
            [search : help:search^ ((require-relative-library "search.ss")
                                    doc-position function)]
            [help : help:help-window^
                  ((require-relative-library "helpwin.ss")
                   info search
                   browser plt-installer
                   function string file url
                   mred framework mixin)])
      (export (open help)
              (var (search doc-collections-changed))))))
