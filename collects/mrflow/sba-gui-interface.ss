
(module sba-gui-interface mzscheme
  (provide
   (struct region-to-color (start end source color))
   )
  
  ; non-negative-int non-negative-int top symbol
  (define-struct region-to-color (start end source color))
  )
