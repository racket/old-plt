
(module pingp-play mzscheme
  (require (lib "pingp-play.ss" "htdp")
           (lib "pingp-sig.ss" "htdp")
           (lib "unitsig.ss"))
  (define-values/invoke-unit/sig pingp-play^ pingp-play@)
  (provide-signature-elements pingp-play^))
	   
