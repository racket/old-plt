
(module launcher mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "compile-sig.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "xml-sig.ss" "xml")
	   (lib "xml.ss" "xml"))
  
  (require "launcher-sig.ss"
	   "launcher-unit.ss")
  
  (define-values/invoke-unit/sig launcher^ 
    launcher@
    #f
    dynext:compile^
    dynext:link^
    xml^)

  (provide-signature-elements launcher^))
