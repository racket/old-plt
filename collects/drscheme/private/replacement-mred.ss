(module replacement-mred mzscheme

  ;; need absolute path here for proper userspace init
  (require (lib "label-frame-mred.ss" "drscheme" "private"))
  
  (provide (all-from-except (lib "label-frame-mred.ss" "drscheme" "private") lookup-frame-name)))
