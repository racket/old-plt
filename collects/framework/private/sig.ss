
(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide framework:menu^
	   framework:menu-class^
	   framework:menu-fun^
	   framework:version^
	   framework:version-class^
	   framework:version-fun^
	   framework:panel^
	   framework:panel-class^
	   framework:panel-fun^
	   framework:exn^
	   framework:exn-class^
	   framework:exn-fun^
	   framework:application^
	   framework:application-class^
	   framework:application-fun^
	   framework:preferences^
	   framework:preferences-class^
	   framework:preferences-fun^
	   framework:autosave^
	   framework:autosave-class^
	   framework:autosave-fun^
	   framework:exit^
	   framework:exit-class^
	   framework:exit-fun^
	   framework:path-utils^
	   framework:path-utils-class^
	   framework:path-utils-fun^
	   framework:finder^
	   framework:finder-class^
	   framework:finder-fun^
	   framework:editor^
	   framework:editor-class^
	   framework:editor-fun^
	   framework:pasteboard^
	   framework:pasteboard-class^
	   framework:pasteboard-fun^
	   framework:text^
	   framework:text-class^
	   framework:text-fun^
	   framework:canvas^
	   framework:canvas-class^
	   framework:canvas-fun^
	   framework:frame^
	   framework:frame-class^
	   framework:frame-fun^
	   framework:group^
	   framework:group-class^
	   framework:group-fun^
	   framework:handler^
	   framework:handler-class^
	   framework:handler-fun^
	   framework:icon^
	   framework:icon-class^
	   framework:icon-fun^
	   framework:keymap^
	   framework:keymap-class^
	   framework:keymap-fun^
	   framework:match-cache^
	   framework:match-cache-class^
	   framework:match-cache-fun^
	   framework:scheme-paren^
	   framework:scheme-paren-class^
	   framework:scheme-paren-fun^
	   framework:scheme^
	   framework:scheme-class^
	   framework:scheme-fun^
	   framework:paren^
	   framework:paren-class^
	   framework:paren-fun^
	   framework:main^
	   framework:main-class^
	   framework:main-fun^
	   framework:color-model^
	   framework:color-model-class^
	   framework:color-model-fun^
           framework:comment-box-fun^
           framework:comment-box-class^
           framework:comment-box^)
           
  (define-signature framework:comment-box-fun^
    ())
  (define-signature framework:comment-box-class^
    (snipclass snip%))
  (define-signature framework:comment-box^
    ((open framework:comment-box-fun^)
     (open framework:comment-box-class^)))
  
  (define-signature framework:menu-class^
    (can-restore<%>
     can-restore-mixin
     can-restore-underscore<%>
     can-restore-underscore-mixin
     can-restore-menu-item%
     can-restore-checkable-menu-item%
     can-restore-underscore-menu%))
  (define-signature framework:menu-fun^
    ())
  (define-signature framework:menu^
    ((open framework:menu-class^)
     (open framework:menu-fun^)))

  (define-signature framework:version-class^
    ())
  (define-signature framework:version-fun^
    (add-spec
     version))
  (define-signature framework:version^
    ((open framework:version-class^)
     (open framework:version-fun^)))

  (define-signature framework:panel-class^
    (single-mixin
     single<%>

     single-window<%>
     single-window-mixin

     ;;multi-view-mixin
     ;;multi-view<%>

     
     single%
     single-pane%
     ;;multi-view%

     dragable<%>
     dragable-mixin

     vertical-dragable<%>
     vertical-dragable-mixin
     vertical-dragable%

     horizontal-dragable<%>
     horizontal-dragable-mixin
     horizontal-dragable%))
  (define-signature framework:panel-fun^
    ())
  (define-signature framework:panel^
    ((open framework:panel-class^)
     (open framework:panel-fun^)))

  (define-signature framework:exn-class^
    ())
  (define-signature framework:exn-fun^
    ((struct exn ())
     (struct unknown-preference ())))
  (define-signature framework:exn^
    ((open framework:exn-class^)
     (open framework:exn-fun^)))

  (define-signature framework:application-class^
    ())
  (define-signature framework:application-fun^
    (current-app-name))
  (define-signature framework:application^
    ((open framework:application-class^)
     (open framework:application-fun^)))

  (define-signature framework:preferences-class^
    ())
  (define-signature framework:preferences-fun^
    (get
     add-callback
     set
     set-default
     set-un/marshall

     save
     read
     restore-defaults

     add-panel
     add-font-panel

     add-editor-checkbox-panel
     add-warnings-checkbox-panel
     add-scheme-checkbox-panel

     add-to-editor-checkbox-panel
     add-to-warnings-checkbox-panel
     add-to-scheme-checkbox-panel
     
     show-dialog
     hide-dialog))
  (define-signature framework:preferences^
    ((open framework:preferences-class^)
     (open framework:preferences-fun^)))

  (define-signature framework:autosave-class^
    (autosavable<%>))
  (define-signature framework:autosave-fun^
    (register
     restore-autosave-files/gui))
  (define-signature framework:autosave^
    ((open framework:autosave-class^)
     (open framework:autosave-fun^)))

  (define-signature framework:exit-class^
    ())
  (define-signature framework:exit-fun^
    (frame-exiting
     insert-on-callback
     insert-can?-callback
     can-exit?
     on-exit
     exit))
  (define-signature framework:exit^
    ((open framework:exit-class^)
     (open framework:exit-fun^)))

  (define-signature framework:path-utils-class^
    ())
  (define-signature framework:path-utils-fun^
    (generate-autosave-name 
     generate-backup-name))
  (define-signature framework:path-utils^
    ((open framework:path-utils-class^)
     (open framework:path-utils-fun^)))

  (define-signature framework:finder-class^
    ())
  (define-signature framework:finder-fun^
    (dialog-parent-parameter
     default-extension
     default-filters
     common-put-file 
     common-get-file 
     std-put-file 
     std-get-file 
     common-get-file-list
     get-file
     put-file))
  (define-signature framework:finder^
    ((open framework:finder-class^)
     (open framework:finder-fun^)))

  (define-signature framework:editor-class^
    (basic<%>
     keymap<%>
     autowrap<%>
     info<%>
     file<%>
     backup-autosave<%>
     basic-mixin
     keymap-mixin
     autowrap-mixin
     info-mixin
     file-mixin
     backup-autosave-mixin))
  (define-signature framework:editor-fun^
    ())
  (define-signature framework:editor^
    ((open framework:editor-class^)
     (open framework:editor-fun^)))

  (define-signature framework:pasteboard-class^
    (basic%
     keymap%
     file%
     backup-autosave%
     info%))
  (define-signature framework:pasteboard-fun^
    ())
  (define-signature framework:pasteboard^
    ((open framework:pasteboard-class^)
     (open framework:pasteboard-fun^)))

  (define-signature framework:text-class^
    (basic<%>
     hide-caret/selection<%>
     delegate<%>
     searching<%>
     return<%>
     info<%>
     clever-file-format<%>
     
     basic% 
     hide-caret/selection%
     1-pixel-string-snip%
     1-pixel-tab-snip%
     delegate%
     keymap%
     return%
     autowrap%
     file%
     clever-file-format%
     backup-autosave%
     searching%
     info%

     basic-mixin
     hide-caret/selection-mixin
     delegate-mixin
     searching-mixin
     return-mixin
     info-mixin
     clever-file-format-mixin))
  (define-signature framework:text-fun^
    ())
  (define-signature framework:text^
    ((open framework:text-class^)
     (open framework:text-fun^)))

  (define-signature framework:canvas-class^
    (basic<%>
     delegate<%>
     info<%>
     wide-snip<%>
     wide-snip%   
     basic%
     info%

     basic-mixin
     delegate-mixin
     info-mixin
     wide-snip-mixin))
  (define-signature framework:canvas-fun^
    ())
  (define-signature framework:canvas^
    ((open framework:canvas-class^)
     (open framework:canvas-fun^)))

  (define-signature framework:frame-class^
    (basic<%>
     standard-menus<%>
     editor<%>
     open-here<%>
     text<%>
     pasteboard<%>
     delegate<%>
     searchable<%>
     searchable-text<%>
     info<%>
     text-info<%>
     pasteboard-info<%>
     file<%>
     basic%
     info%
     text-info%
     pasteboard-info%
     standard-menus%
     editor%
     open-here%
     text%
     text-info-file%
     searchable%
     delegate%
     pasteboard%
     pasteboard-info-file%
     
     basic-mixin
     standard-menus-mixin
     editor-mixin
     open-here-mixin
     text-mixin
     pasteboard-mixin
     delegate-mixin
     searchable-mixin
     searchable-text-mixin
     info-mixin
     text-info-mixin
     pasteboard-info-mixin
     file-mixin))
  (define-signature framework:frame-fun^
    (reorder-menus
     add-snip-menu-items))
  (define-signature framework:frame^
    ((open framework:frame-class^)
     (open framework:frame-fun^)))

  (define-signature framework:group-class^
    (%))
  (define-signature framework:group-fun^
    (get-the-frame-group))
  (define-signature framework:group^
    ((open framework:group-class^)
     (open framework:group-fun^)))

  (define-signature framework:handler-class^
    ())
  (define-signature framework:handler-fun^
    (handler? 
     handler-name 
     handler-extension
     handler-handler
     insert-format-handler
     find-format-handler 
     find-named-format-handler 
     current-create-new-window
     edit-file
     open-file
     install-recent-items
     add-to-recent
     set-recent-position
     set-recent-items-frame-superclass
     size-recently-opened-files))
  (define-signature framework:handler^
    ((open framework:handler-class^)
     (open framework:handler-fun^)))

  (define-signature framework:icon-class^
    ())
  (define-signature framework:icon-fun^
    (get-paren-highlight-bitmap
     get-autowrap-bitmap

     get-lock-bitmap
     get-unlock-bitmap
     get-anchor-bitmap

     get-left/right-cursor
     get-up/down-cursor
     
     get-gc-on-bitmap
     get-gc-off-bitmap))
  (define-signature framework:icon^
    ((open framework:icon-class^)
     (open framework:icon-fun^)))

  (define-signature framework:keymap-class^
    (aug-keymap%
     aug-keymap<%>
     aug-keymap-mixin))
  (define-signature framework:keymap-fun^
    (send-map-function-meta
     make-meta-prefix-list

     canonicalize-keybinding-string

     add-to-right-button-menu
     add-to-right-button-menu/before

     setup-global
     setup-search
     setup-file
     setup-editor

     get-global
     get-search
     get-file
     get-editor

     call/text-keymap-initializer))
  (define-signature framework:keymap^
    ((open framework:keymap-class^)
     (open framework:keymap-fun^)))

  (define-signature framework:match-cache-class^
    (%))
  (define-signature framework:match-cache-fun^
    ())
  (define-signature framework:match-cache^
    ((open framework:match-cache-class^)
     (open framework:match-cache-fun^)))

  (define-signature framework:scheme-paren-class^
    ())
  (define-signature framework:scheme-paren-fun^
    (get-comments
     get-paren-pairs
     get-quote-pairs
     forward-match
     backward-match
     balanced?
     backward-containing-sexp))
  (define-signature framework:scheme-paren^
    ((open framework:scheme-paren-class^)
     (open framework:scheme-paren-fun^)))

  (define-signature framework:scheme-class^
    (text<%>
     text%
     text-mixin
     sexp-snip%
     sexp-snip<%>))
  (define-signature framework:scheme-fun^
    (get-wordbreak-map
     init-wordbreak-map
     get-style-list
     get-keymap
     setup-keymap
     add-preferences-panel))
  (define-signature framework:scheme^
    ((open framework:scheme-class^)
     (open framework:scheme-fun^)))

  (define-signature framework:paren-class^
    ())
  (define-signature framework:paren-fun^
    (balanced? 
     forward-match 
     backward-match
     skip-whitespace))
  (define-signature framework:paren^
    ((open framework:paren-class^)
     (open framework:paren-fun^)))

  (define-signature framework:main-class^ ())
  (define-signature framework:main-fun^ ())
  (define-signature framework:main^
    ((open framework:main-class^)
     (open framework:main-fun^)))

  (define-signature framework:color-model-class^
    ())
  (define-signature framework:color-model-fun^
    (xyz?
     xyz-x
     xyz-y
     xyz-z
     rgb-color-distance
     rgb->xyz
     xyz->rgb))
  (define-signature framework:color-model^
    ((open framework:color-model-class^)
     (open framework:color-model-fun^))))
