(module contracts mzscheme
  (require "contract.ss")
  (fprintf
   (current-error-port)
   "WARNING: (lib \"contracts.ss\") is deprecated. Use (lib \"contract.ss\") instead\n")
  (provide (all-from "contract.ss")))