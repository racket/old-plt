#cs(module pingp mzscheme
     (require (lib "pingp.ss" "htdp")
              (lib "pingp-sig.ss" "htdp")
              (lib "draw-sig.ss" "htdp")
              (lib "big-draw.ss" "htdp")
              (lib "unitsig.ss"))
     
     (provide-signature-elements pingpS)
     (provide-signature-elements draw^)
     (define-values/invoke-unit/sig pingpS pingpU))
