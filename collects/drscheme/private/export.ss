(module export mzscheme
  (require (lib "unitsig.ss")
           "snip.ss"
           "load-handler.ss"
           "rep.ss"
           "frame.ss"
           "unit.ss"
           "get-extend.ss"
           "language.ss"
           "help-info.ss"
           "help-interface.ss")

  (provide export@)
  
  (define export@
    (compound-unit/sig
      (import [app : drscheme:app^]
              [text : drscheme:text^]
              [init : drscheme:init^]
              [aries : plt:aries^])
      (link [snip : drscheme:snip^ (snip@)]
            [interface : drscheme:interface^ (interface@ aries zodiac)]
            [basis : plt:basis^ (basis@ basis-import params interface aries)]
            [load-handler : drscheme:load-handler^ (load-handler@ basis)]
            [rep : drscheme:rep^
                 (rep@ interface init snip language app frame unit
                  basis text load-handler help-interface)]
            [frame : drscheme:frame^
                   (frame@ unit app help-interface)]
            [unit : drscheme:unit^
                  (unit@ launcher basis help-interface app frame text rep language get/extend snip)]
            [get/extend : drscheme:get/extend^ (get-extend@ frame rep)]
            [language : drscheme:language^ (language@ unit basis)]            
            [help-info : help:get-info^ (help-info@ basis language)]
            [help-interface : help:drscheme-interface^ (help-interface@ frame language basis)])
      (export (unit snip)
              (unit interface)
              (unit basis)
              (unit frame)
              (unit unit)
              (unit get/extend)
              (unit load-handler)
              (unit rep)
              (unit help-interface)
              (unit language)
              (unit help-info)))))
