(begin-elaboration-time
 (if mred:explicit-wx?
     `(reference "wxs.ss")
     `(define-signature mred:wx^ ())))

(define-signature mred:connections^
  (connections-frame%
   connections-dialog-box%
   connections-media-edit%
   connections-media-pasteboard%
   connections-media-canvas%
   connections-panel%

   active-frame

   make-connections-frame%
   make-connections-media-buffer%
   make-connections-media-canvas%
   make-connections-panel%))

(define-signature mred:version^
  (add-version-spec
   version))

(define-signature mred:html^
  (html-convert))

(define-signature mred:panel^
  (make-edit-panel%
   horizontal-edit-panel%
   vertical-edit-panel%))

(define-signature mred:url^
  ((struct url (scheme host port path params query fragment))
   unixpath->path
   get-pure-port			; url [x list (str)] -> in-port
   get-impure-port			; url [x list (str)] -> in-port
   display-pure-port			; in-port -> ()
   purify-port				; in-port -> list (mime-header)
   netscape/string->url			; (string -> url)
   string->url				; str -> url
   url->string
   call/input-url			; url x (url -> in-port) x
					; (in-port -> ())
					; [x list (str)] -> ()
   combine-url/relative))		; url x str -> url

(define-signature mred:exn^
  ((struct exn ())
   (struct exn:unknown-preference ())
   (struct exn:during-preferences ())
   (struct exn:url ())))

(define-signature mred:container-children^
  (const-default-size
   const-default-posn
   const-default-spacing
   const-default-border
   (struct child-info (x-posn y-posn x-min y-min x-margin y-margin x-stretch y-stretch))
   get-two-int-values
   non-negative-number?
   same-dimension?
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

(define-signature mred:hyper-loader^
  (open-hyper-make
   open-hyper-view
   hyper-text-require))

(define-signature mred:application^
  (console
   app-name
   eval-string))

(define-signature mred:html-mode^
  ())

(define-signature mred:debug^ (printf exit? on?))

(define-signature mred:constants^
  (debug-on
   debug-param
   plt-home-directory
   original-input-port
   original-output-port))

(define-signature mred:exn-external^
  (exn? exn:unknown-preference? exn:during-preferences? exn:url?))

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

(define-signature mred:container-frames^
  (frame% dialog-box%))

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

(define-signature mred:container-panels^
  (debug-borders
   panel%
   horizontal-panel%
   vertical-panel%
   single-panel%))

(define-signature mred:autoload^
  (make-autoload))

(define-signature mred:autosave^
  (register-autosave))

(define-signature mred:exit^
  (insert-exit-callback
   remove-exit-callback
   run-exit-callbacks
   exit))

(define-signature mred:gui-utils^
  (get-font-from-user
   get-colour-from-user
   message-box
   cursor-delay
   show-busy-cursor
   delay-action
   local-busy-cursor
   get-choice
   unsaved-warning
   read-snips/chars-from-buffer
   open-input-buffer
   print-paper-names))

(define-signature mred:console^
  (credits-proc
   copyright-string
   welcome-message
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
   make-return-edit%
   edit%
   return-edit%
   pasteboard%
   
   make-snip%
   snip%
   media-snip%))

(define-signature mred:canvas^
  (make-wrapping-canvas%
   wrapping-canvas%

   make-one-line-canvas%
   one-line-canvas%

   make-frame-title-canvas%
   frame-title-canvas%
   
   make-wide-snip-canvas%
   wide-snip-canvas%

   number-control%))

(define-signature mred:frame^
  (frame-width
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
   current-frames
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
   open-url
   open-file))

(define-signature mred:icon^
  (icon
   paren-highlight-bitmap
   autowrap-bitmap
   reset-console-bitmap))

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
   scheme-interaction-mode%
   scheme-mode-style-list))

(define-signature mred:paren^
  (balanced? 
   forward-match 
   backward-match
   skip-whitespace))


(define-signature mred:hyper-edit^
  ((struct hypertag (name position))
   (struct hyperlink (anchor-start anchor-end url-string))
   hyper-buffer-data%
   hyper-data-class
   make-hyper-edit%
   hyper-edit%))

(define-signature mred:hyper-dialog^
  (hyper-tag-dialog%
   hyper-get-current-tags))

(define-signature mred:hyper-frame^
  (hyper-frame-group
   make-hyper-canvas%
   hyper-canvas%
   make-hyper-basic-frame%
   hyper-basic-frame%
   make-hyper-view-frame%
   hyper-view-frame%
   make-hyper-make-frame%
   hyper-make-frame%
   open-hyper-view
   open-hyper-make
   hyper-text-require))

(define-signature mred:container^
  ((open mred:container-frames^)
   (open mred:container-children-export^)
   (open mred:container-panels^)))

(define-signature mred^
  ((unit constants : mred:constants^)
   (open mred:version^)
   (open mred:exn-external^)
   (open mred:connections^) (open mred:container^) (open mred:preferences^)
   (open mred:autoload^) (open mred:autosave^) (open mred:exit^)
   (open mred:gui-utils^) (open mred:console^) (open mred:path-utils^)
   (open mred:finder^)
   (open mred:find-string^) (open mred:edit^) (open mred:canvas^)
   (open mred:frame^) (open mred:editor-frame^)
   (open mred:group^) (open mred:handler^) (open mred:icon^) (open mred:keymap^)
   (open mred:match-cache^) (open mred:menu^) (open mred:mode^) 
   (open mred:panel^) (open mred:paren^) (open mred:project^)
   (open mred:scheme-paren^) (open mred:scheme-mode^) 
   (open mred:hyper-edit^) (open mred:hyper-dialog^) (open mred:hyper-frame^)
   (open mred:url^)))
