(module slatex-wrapper mzscheme
  
  (require (lib "include.ss"))
  
  (provide slatex::process-main-tex-file)
  
  (include "slatex.scm"))