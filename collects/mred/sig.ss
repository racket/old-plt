(define-signature mred:debug^ (printf exit?))

(define-signature mred:application^
  (console-frame
   eval-string))

(define-signature mred:exn^
  ((struct exn ())
   (struct exn:unknown-preference ())))

(define-signature mred:exn-external^
  (exn? exn:unknown-preference?))

(define-signature mred:preferences^
  (get-preference
   add-preference-callback
   set-preference
   set-preference-default
   set-preference-un/marshall

   save-user-preferences
   read-user-preferences
   restore-defaults

   add-preference-panel
   show-preferences-dialog
   hide-preferences-dialog))

(define-signature mred:container-children^
  (const-default-size
   const-default-posn
   const-default-spacing
   const-default-border
   (struct child-info (x-posn y-posn x-min y-min x-stretch y-stretch))
   get-two-int-values
   non-negative-number?
   make-item%
   button%
   check-box%
   choice%
   gauge%
   list-box%
   message%
   radio-box%
   slider%
   text%
   multi-text%
   canvas%
   media-canvas%
   text-window%))

(define-signature mred:container-frames^
  (frame% dialog-box%))

(define-signature mred:container-panels^
  (panel%
   horizontal-panel%
   vertical-panel%
   single-panel%))

(define-signature mred:container-children-export^
  (const-default-size
   const-default-posn
   const-default-spacing
   const-default-border
   (struct child-info (x-posn y-posn x-min y-min x-stretch y-stretch))
   button%
   check-box%
   choice%
   gauge%
   list-box%
   message%
   radio-box%
   slider%
   text%
   multi-text%
   canvas%
   media-canvas%
   text-window%))

(define-signature mred:container^
  ((open mred:container-frames^)
   (open mred:container-children-export^)
   (open mred:container-panels^)))
;  (const-default-size
;   const-default-posn
;   const-default-spacing
;   (struct child-info (x-posn y-posn x-min y-min x-stretch y-stretch))
;   frame%
;   dialog-box%
;   canvas%
;   media-canvas%
;   button%
;   check-box%
;   choice%
;   gauge%
;   list-box%
;   message%
;   radio-box%
;   slider%
;   text-window%
;   text%
;   multi-text%
;   panel%
;   horizontal-panel%
;   vertical-panel%
;   single-panel%))


(define-signature mred:autoload^
  (make-autoload))

(define-signature mred:autosave^
  (register-autosave))

(define-signature mred:exit^
  (insert-exit-callback
   remove-exit-callback
   exit))

(define-signature mred:gui-utils^
  (cursor-delay
   show-busy-cursor
   delay-action
   local-busy-cursor
   get-choice
   unsaved-warning
   open-input-buffer
   print-paper-names))

(define-signature mred:console^
  (welcome-message
   console-max-save-previous-exprs
   
   make-scheme-mode-edit%
   scheme-mode-edit%
   
   make-console-edit%
   console-edit%
   
   make-console-frame%
   console-frame%))

(define-signature mred:path-utils^
  (generate-autosave-name 
   generate-backup-name))

(define-signature mred:finder^
  (filter-match?
   common-put-file 
   common-get-file 
   std-put-file 
   std-get-file 
   common-get-file-list
   current-find-file-directory
   get-file
   put-file))

(define-signature mred:find-string^
  (make-find-frame%
   find-frame%
   find-string
   make-searchable-frame%))

(define-signature mred:edit^
  (make-std-buffer%
   make-edit%
   make-pasteboard%
   edit%
   pasteboard%))

(define-signature mred:canvas^
  (make-editor-canvas%
   make-simple-frame-canvas%
   editor-canvas%
   simple-frame-canvas%))

(define-signature mred:frame^
  (frame-name
   frame-width
   frame-height

   make-simple-frame%
   make-menu-frame%
   make-standard-menus-frame%

   empty-frame%
   menu-frame%
   standard-menus-frame%
   simple-menu-frame%))

(define-signature mred:editor-frame^
  (make-editor-frame%
   editor-frame%
   make-pasteboard-frame%
   pasteboard-frame%
   make-status-frame%))

(define-signature mred:group^
  (buffer-group%
   frame-group%
   frames
   keep-frames))

(define-signature mred:handler^
  (handler? handler-name handler-extension handler-handler
   format-handlers mode-handlers
   insert-format-handler
   insert-mode-handler
   find-format-handler 
   find-mode-handler
   find-named-format-handler 
   find-named-mode-handler
   edit-file
   open-file))

(define-signature mred:icon^
  (icon
   paren-highlight-bitmap
   autowrap-bitmap))

(define-signature mred:keymap^
  (keyerr
   set-keymap-error-handler
   shifted-key-list
   set-keymap-implied-shifts
   make-meta-prefix-list
   send-map-function-meta
   setup-global-keymap
   global-keymap))

(define-signature mred:match-cache^
  (match-cache%))

(define-signature mred:menu^
  (max-manual-menu-id
   generate-menu-id
   make-menu%
   menu%
   make-menu-bar%
   menu-bar%))

(define-signature mred:mode^
  (basic-mode%
  make-mode%
  mode%))

(define-signature mred:project^
  (project-frame-group%
   make-project-frame%
   project-frame%))

(define-signature mred:scheme-paren^
  (scheme-paren-pairs
   scheme-quote-pairs
   scheme-comments
   scheme-forward-match
   scheme-backward-match
   scheme-balanced?
   scheme-backward-containing-sexp))

(define-signature mred:scheme-mode^
  (scheme-mode-tabify-on-return?
   scheme-mode-match-round-to-square?
   scheme-media-wordbreak-map
   scheme-init-wordbreak-map
   setup-global-scheme-mode-keymap
   setup-global-scheme-interaction-mode-keymap
   global-scheme-mode-keymap
   global-scheme-interaction-mode-keymap
   make-scheme-mode%
   make-scheme-interaction-mode%
   scheme-mode%
   scheme-interaction-mode%))

(define-signature mred:paren^
  (balanced? 
   forward-match 
   backward-match
   skip-whitespace))

(define-signature mred:hyper-loader^
  (open-hyper-make
   open-hyper-view
   hyper-text-require))
