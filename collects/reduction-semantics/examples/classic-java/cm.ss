(module cm mzscheme

  (require (lib "cm.ss"))

  (current-load/use-compiled
   (make-compilation-manager-load/use-compiled-handler)))
