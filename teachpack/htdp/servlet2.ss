#cs(module servlet2 mzscheme
  (require (lib "servlet-primitives.ss" "web-server")
           (lib "servlet2-unit.ss" "htdp")
           (lib "servlet-sig.ss" "web-server")
	   (lib "process.ss")
           (lib "unitsig.ss"))
  
  (provide-signature-elements servlet2^)
  
  (define-values/invoke-unit/sig
   servlet2^
   (compound-unit/sig
    (import)
    (link
     [S1 : servlet^ (servlet@)]
     [S2 : servlet2^ (servlet2@ S1)])
    (export (open S2)))
   #f)
  
  )
