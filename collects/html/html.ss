;; copyright by Paul Graunke June 2000 AD

(module html mzscheme
  (require (lib "unitsig.ss")
	   "html-sig.ss" 
	   "html-unit.ss"
	   "sgml-reader-sig.ss"
	   "sgml-reader-unit.ss"
	   (lib "xml.ss" "xml")
	   (lib "xml-sig.ss" "xml")
	   (lib "sig.ss" "xml" "private")
	   (lib "xml-unit.ss" "xml"))

  (define-values/invoke-unit/sig
    html^
    (compound-unit/sig
     (import [x : xml^])
     (link
      [s : sgml-reader^ (sgml-reader@ (x : xml-structs^))]
      [h : html^ (html@ x s)])
     (export (open h)))
    #f
    xml^)

  (provide-signature-elements html^))
