(module plot-extend mzscheme
  (require
   (lib "view.ss" "plot")
   (lib "renderer-helpers.ss" "plot"))
  
  (provide 
   (all-from (lib "view.ss" "plot"))
   (all-from (lib "renderer-helpers.ss" "plot"))))