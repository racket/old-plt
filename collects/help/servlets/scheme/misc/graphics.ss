(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (TITLE "How to write graphics programs")
   (HEAD ,hd-css)
   (BODY 
    (H1 "How to write graphics programs")
    (A ((NAME "gfx") (VALUE "Graphics")))
    (A ((NAME "gui") (VALUE "GUIs")))
    (A ((NAME "gui2") (VALUE "Graphical User Interfaces")))
    "To write graphics programs, use DrScheme with the "
    "Graphical (MrEd) flavor of the PLT " 
    (A ((HREF "/doc/help/scheme/what.ss")) " language") ". "
    "MrEd provides a complete GUI toolbox that is described "
    "in " 
    (A ((HREF "/doc/mred/")) "PLT MrEd: Graphical Toolbox Manual") ". "
    (P)
    "For simple graphics programs, you may also use the "
    "viewport-based graphics library, which is described in "
    ,(manual-entry "misclib" "viewport" "Viewport Graphics") ". "
    "The following declaration loads viewport graphics into MrEd:"
    (PRE " (require (lib \"graphics.ss\" \"graphics\"))"))))
