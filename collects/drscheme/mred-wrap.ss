(module mred-wrap mzscheme
  (require (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred"))
  (provide-signature-elements mred^))