(module export mzscheme
  (require (lib "unitsig.ss")
	   "drsig.ss"
           "snip.ss"
           "load-handler.ss"
           "rep.ss"
           "frame.ss"
           "unit.ss"
           "get-extend.ss"
           "language.ss"
           "help-interface.ss")

  (provide export@)
  
  (define export@
    (compound-unit/sig
      (import [app : drscheme:app^]
              [text : drscheme:text^]
              [init : drscheme:init^])
      (link [snip : drscheme:snip^ (snip@)]
            [load-handler : drscheme:load-handler^ (load-handler@)]
            [rep : drscheme:rep^
                 (rep@ init snip language app frame unit text load-handler help-interface)]
            [frame : drscheme:frame^
                   (frame@ unit app help-interface)]
            [unit : drscheme:unit^
                  (unit@ help-interface app frame text rep language get/extend snip)]
            [get/extend : drscheme:get/extend^ (get-extend@ frame rep)]
            [language : drscheme:language^ (language@ unit)]            
            [help-interface : drscheme:help-interface^ (help-interface@ frame language)])
      (export (unit snip)
              (unit frame)
              (unit unit)
              (unit get/extend)
              (unit load-handler)
              (unit rep)
              (unit help-interface)
              (unit language)))))
