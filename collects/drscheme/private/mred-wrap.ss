(module mred-wrap mzscheme
  (require (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred"))
  (provide-signature-elements mred^))