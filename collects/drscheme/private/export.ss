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
            [load-handler : drscheme:load-handler^ (load-handler@)]
            [rep : drscheme:rep^
                 (rep@ interface init snip language app frame unit text load-handler help-interface)]
            [frame : drscheme:frame^
                   (frame@ unit app help-interface)]
            [unit : drscheme:unit^
                  (unit@ launcher help-interface app frame text rep language get/extend snip)]
            [get/extend : drscheme:get/extend^ (get-extend@ frame rep)]
            [language : drscheme:language^ (language@ unit)]            
            [help-info : help:get-info^ (help-info@ language)]
            [help-interface : help:drscheme-interface^ (help-interface@ frame language)])
      (export (unit snip)
              (unit interface)
              (unit frame)
              (unit unit)
              (unit get/extend)
              (unit load-handler)
              (unit rep)
              (unit help-interface)
              (unit language)
              (unit help-info)))))
