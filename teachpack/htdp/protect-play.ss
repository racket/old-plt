
(module protect-play mzscheme
  (require (lib "protect-play.ss" "htdp")
           (lib "pingp-sig.ss" "htdp")
           (lib "unitsig.ss"))
  (define-values/invoke-unit/sig protect-play^ protect-play@)
  (provide-signature-elements protect-play^))

