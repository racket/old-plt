
(module info (lib "infotab.ss" "setup")
  (define name "Slideshow")
  (define mred-launcher-libraries (list "start.ss"))
  (define mred-launcher-names (list "Slideshow"))
  (define tools (list '("tool.ss")))
  (define tool-names (list "Slideshow"))
  (define tool-icons (list (list "slideshow.bmp" "slideshow")))
  (define compile-omit-files
    (list "tutorial-show.ss" "initial-ones.ss" "pict-snipclass.ss")))
