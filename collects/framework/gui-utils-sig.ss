(module guiutils-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide framework:gui-utils^)
  
  (define-signature framework:gui-utils^
    (next-untitled-name
     cursor-delay
     show-busy-cursor
     delay-action
     local-busy-cursor
     unsaved-warning
     text-snip<%>
     read-snips/chars-from-text
     get-choice
     open-input-buffer
     get-clicked-clickback-delta
     get-clickback-delta)))
