(define-signature
  mred-interfaces^
  ((open mred^)
   vertical-panel<%>
   vertical-pane<%>
   timer<%>
   text-field<%>
   text<%>
   tab-snip<%>
   style-list<%>
   style-delta<%>
   string-snip<%>
   snip-class<%>
   snip-admin<%>
   snip<%>
   slider<%>
   separator-menu-item<%>
   scroll-event<%>
   radio-box<%>
   ps-setup<%>
   printer-dc<%>
   post-script-dc<%>
   popup-menu<%>
   point<%>
   pen-list<%>
   pen<%>
   pasteboard<%>
   panel<%>
   pane<%>
   mouse-event<%>
   meta-file-dc<%>
   message<%>
   menu-bar<%>
   menu<%>
   memory-dc<%>
   list-box<%>
   keymap<%>
   key-event<%>
   image-snip<%>
   icon<%>
   horizontal-panel<%>
   horizontal-pane<%>
   gauge<%>
   frame<%>
   font-list<%>
   font<%>
   event<%>
   editor-wordbreak-map<%>
   editor-stream-out-string-base<%>
   editor-stream-out-base<%>
   editor-stream-out<%>
   editor-stream-in-string-base<%>
   editor-stream-in-base<%>
   editor-stream-in<%>
   editor-snip<%>
   editor-data-class-list<%>
   editor-data-class<%>
   editor-data<%>
   editor-canvas<%>
   editor-admin<%>
   dialog<%>
   cursor<%>
   control-event<%>
   color<%>
   clipboard-client<%>
   choice<%>
   checkable-menu-item<%>
   check-box<%>
   canvas-dc<%>
   button<%>
   brush-list<%>
   brush<%>
   bitmap<%>))

(define mred-interfaces@
  (compound-unit/sig
    (import)
    (link (mred : mred^ (mred@))
          (interfaces
            :
            (vertical-panel<%>
              vertical-pane<%>
              timer<%>
              text-field<%>
              text<%>
              tab-snip<%>
              style-list<%>
              style-delta<%>
              string-snip<%>
              snip-class<%>
              snip-admin<%>
              snip<%>
              slider<%>
              separator-menu-item<%>
              scroll-event<%>
              radio-box<%>
              ps-setup<%>
              printer-dc<%>
              post-script-dc<%>
              popup-menu<%>
              point<%>
              pen-list<%>
              pen<%>
              pasteboard<%>
              panel<%>
              pane<%>
              mouse-event<%>
              meta-file-dc<%>
              message<%>
              menu-bar<%>
              menu<%>
              memory-dc<%>
              list-box<%>
              keymap<%>
              key-event<%>
              image-snip<%>
              icon<%>
              horizontal-panel<%>
              horizontal-pane<%>
              gauge<%>
              frame<%>
              font-list<%>
              font<%>
              event<%>
              editor-wordbreak-map<%>
              editor-stream-out-string-base<%>
              editor-stream-out-base<%>
              editor-stream-out<%>
              editor-stream-in-string-base<%>
              editor-stream-in-base<%>
              editor-stream-in<%>
              editor-snip<%>
              editor-data-class-list<%>
              editor-data-class<%>
              editor-data<%>
              editor-canvas<%>
              editor-admin<%>
              dialog<%>
              cursor<%>
              control-event<%>
              color<%>
              clipboard-client<%>
              choice<%>
              checkable-menu-item<%>
              check-box<%>
              canvas-dc<%>
              button<%>
              brush-list<%>
              brush<%>
              bitmap<%>
              vertical-panel%
              vertical-pane%
              timer%
              text-field%
              text%
              tab-snip%
              style-list%
              style-delta%
              string-snip%
              snip-class%
              snip-admin%
              snip%
              slider%
              separator-menu-item%
              scroll-event%
              radio-box%
              ps-setup%
              printer-dc%
              post-script-dc%
              popup-menu%
              point%
              pen-list%
              pen%
              pasteboard%
              panel%
              pane%
              mouse-event%
              meta-file-dc%
              message%
              menu-bar%
              menu%
              memory-dc%
              list-box%
              keymap%
              key-event%
              image-snip%
              icon%
              horizontal-panel%
              horizontal-pane%
              gauge%
              frame%
              font-list%
              font%
              event%
              editor-wordbreak-map%
              editor-stream-out-string-base%
              editor-stream-out-base%
              editor-stream-out%
              editor-stream-in-string-base%
              editor-stream-in-base%
              editor-stream-in%
              editor-snip%
              editor-data-class-list%
              editor-data-class%
              editor-data%
              editor-canvas%
              editor-admin%
              dialog%
              cursor%
              control-event%
              color%
              clipboard-client%
              choice%
              checkable-menu-item%
              check-box%
              canvas-dc%
              button%
              brush-list%
              brush%
              bitmap%)
            ((unit/sig
               (vertical-panel<%>
                 vertical-pane<%>
                 timer<%>
                 text-field<%>
                 text<%>
                 tab-snip<%>
                 style-list<%>
                 style-delta<%>
                 string-snip<%>
                 snip-class<%>
                 snip-admin<%>
                 snip<%>
                 slider<%>
                 separator-menu-item<%>
                 scroll-event<%>
                 radio-box<%>
                 ps-setup<%>
                 printer-dc<%>
                 post-script-dc<%>
                 popup-menu<%>
                 point<%>
                 pen-list<%>
                 pen<%>
                 pasteboard<%>
                 panel<%>
                 pane<%>
                 mouse-event<%>
                 meta-file-dc<%>
                 message<%>
                 menu-bar<%>
                 menu<%>
                 memory-dc<%>
                 list-box<%>
                 keymap<%>
                 key-event<%>
                 image-snip<%>
                 icon<%>
                 horizontal-panel<%>
                 horizontal-pane<%>
                 gauge<%>
                 frame<%>
                 font-list<%>
                 font<%>
                 event<%>
                 editor-wordbreak-map<%>
                 editor-stream-out-string-base<%>
                 editor-stream-out-base<%>
                 editor-stream-out<%>
                 editor-stream-in-string-base<%>
                 editor-stream-in-base<%>
                 editor-stream-in<%>
                 editor-snip<%>
                 editor-data-class-list<%>
                 editor-data-class<%>
                 editor-data<%>
                 editor-canvas<%>
                 editor-admin<%>
                 dialog<%>
                 cursor<%>
                 control-event<%>
                 color<%>
                 clipboard-client<%>
                 choice<%>
                 checkable-menu-item<%>
                 check-box<%>
                 canvas-dc<%>
                 button<%>
                 brush-list<%>
                 brush<%>
                 bitmap<%>
                 vertical-panel%
                 vertical-pane%
                 timer%
                 text-field%
                 text%
                 tab-snip%
                 style-list%
                 style-delta%
                 string-snip%
                 snip-class%
                 snip-admin%
                 snip%
                 slider%
                 separator-menu-item%
                 scroll-event%
                 radio-box%
                 ps-setup%
                 printer-dc%
                 post-script-dc%
                 popup-menu%
                 point%
                 pen-list%
                 pen%
                 pasteboard%
                 panel%
                 pane%
                 mouse-event%
                 meta-file-dc%
                 message%
                 menu-bar%
                 menu%
                 memory-dc%
                 list-box%
                 keymap%
                 key-event%
                 image-snip%
                 icon%
                 horizontal-panel%
                 horizontal-pane%
                 gauge%
                 frame%
                 font-list%
                 font%
                 event%
                 editor-wordbreak-map%
                 editor-stream-out-string-base%
                 editor-stream-out-base%
                 editor-stream-out%
                 editor-stream-in-string-base%
                 editor-stream-in-base%
                 editor-stream-in%
                 editor-snip%
                 editor-data-class-list%
                 editor-data-class%
                 editor-data%
                 editor-canvas%
                 editor-admin%
                 dialog%
                 cursor%
                 control-event%
                 color%
                 clipboard-client%
                 choice%
                 checkable-menu-item%
                 check-box%
                 canvas-dc%
                 button%
                 brush-list%
                 brush%
                 bitmap%)
               (import mred^)
               (rename
                 (-vertical-panel% vertical-panel%)
                 (-vertical-pane% vertical-pane%)
                 (-timer% timer%)
                 (-text-field% text-field%)
                 (-text% text%)
                 (-tab-snip% tab-snip%)
                 (-style-list% style-list%)
                 (-style-delta% style-delta%)
                 (-string-snip% string-snip%)
                 (-snip-class% snip-class%)
                 (-snip-admin% snip-admin%)
                 (-snip% snip%)
                 (-slider% slider%)
                 (-separator-menu-item% separator-menu-item%)
                 (-scroll-event% scroll-event%)
                 (-radio-box% radio-box%)
                 (-ps-setup% ps-setup%)
                 (-printer-dc% printer-dc%)
                 (-post-script-dc% post-script-dc%)
                 (-popup-menu% popup-menu%)
                 (-point% point%)
                 (-pen-list% pen-list%)
                 (-pen% pen%)
                 (-pasteboard% pasteboard%)
                 (-panel% panel%)
                 (-pane% pane%)
                 (-mouse-event% mouse-event%)
                 (-meta-file-dc% meta-file-dc%)
                 (-message% message%)
                 (-menu-bar% menu-bar%)
                 (-menu% menu%)
                 (-memory-dc% memory-dc%)
                 (-list-box% list-box%)
                 (-keymap% keymap%)
                 (-key-event% key-event%)
                 (-image-snip% image-snip%)
                 (-icon% icon%)
                 (-horizontal-panel% horizontal-panel%)
                 (-horizontal-pane% horizontal-pane%)
                 (-gauge% gauge%)
                 (-frame% frame%)
                 (-font-list% font-list%)
                 (-font% font%)
                 (-event% event%)
                 (-editor-wordbreak-map% editor-wordbreak-map%)
                 (-editor-stream-out-string-base%
                   editor-stream-out-string-base%)
                 (-editor-stream-out-base% editor-stream-out-base%)
                 (-editor-stream-out% editor-stream-out%)
                 (-editor-stream-in-string-base% editor-stream-in-string-base%)
                 (-editor-stream-in-base% editor-stream-in-base%)
                 (-editor-stream-in% editor-stream-in%)
                 (-editor-snip% editor-snip%)
                 (-editor-data-class-list% editor-data-class-list%)
                 (-editor-data-class% editor-data-class%)
                 (-editor-data% editor-data%)
                 (-editor-canvas% editor-canvas%)
                 (-editor-admin% editor-admin%)
                 (-dialog% dialog%)
                 (-cursor% cursor%)
                 (-control-event% control-event%)
                 (-color% color%)
                 (-clipboard-client% clipboard-client%)
                 (-choice% choice%)
                 (-checkable-menu-item% checkable-menu-item%)
                 (-check-box% check-box%)
                 (-canvas-dc% canvas-dc%)
                 (-button% button%)
                 (-brush-list% brush-list%)
                 (-brush% brush%)
                 (-bitmap% bitmap%))
               (define vertical-panel<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   on-focus
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   get-client-size
                   focus
                   is-enabled?
                   border
                   get-alignment
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define vertical-pane<%>
                 (interface
                   ()
                   get-parent
                   border
                   get-alignment
                   get-top-level-window
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child))
               (define timer<%> (interface () interval notify start stop))
               (define text-field<%>
                 (interface
                   ()
                   on-drop-file
                   get-value
                   set-value
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-editor
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define text<%>
                 (interface
                   ()
                   cut
                   print
                   clear
                   delete
                   find-string
                   load-file
                   save-file
                   set-cursor
                   refresh
                   set-anchor
                   get-anchor
                   flash-on
                   flash-off
                   erase
                   paste-next
                   do-copy
                   do-paste
                   split-snip
                   find-line
                   line-length
                   last-line
                   find-snip
                   get-text
                   get-tabs
                   set-tabs
                   can-insert?
                   on-insert
                   can-delete?
                   on-delete
                   hide-caret
                   insert
                   copy
                   paste
                   kill
                   copy-self
                   own-caret
                   blink-caret
                   on-focus
                   on-change
                   scroll-to
                   resized
                   on-new-box
                   get-file
                   put-file
                   insert-file
                   get-extent
                   get-descent
                   get-space
                   print-to-dc
                   get-admin
                   set-admin
                   select-all
                   undo
                   redo
                   clear-undos
                   set-keymap
                   get-keymap
                   lock
                   is-locked?
                   insert-box
                   on-paint
                   on-event
                   on-char
                   get-dc
                   global-to-local
                   local-to-global
                   get-view-size
                   do-edit-operation
                   get-style-list
                   set-style-list
                   is-modified?
                   get-filename
                   insert-image
                   get-focus-snip
                   set-position
                   set-position-bias-scroll
                   get-visible-position-range
                   get-visible-line-range
                   find-position-in-line
                   get-between-threshold
                   set-between-threshold
                   paragraph-start-position
                   paragraph-end-position
                   paragraph-start-line
                   can-set-size-constraint?
                   on-set-size-constraint
                   after-set-size-constraint
                   read-header-from-file
                   read-footer-from-file
                   write-headers-to-file
                   write-footers-to-file
                   invalidate-bitmap-cache
                   scroll-line-location
                   get-start-position
                   get-end-position
                   move-position
                   scroll-to-position
                   find-position
                   position-line
                   position-location
                   line-location
                   line-start-position
                   line-end-position
                   last-position
                   position-paragraph
                   line-paragraph
                   paragraph-end-line
                   last-paragraph
                   find-string-all
                   get-snip-position
                   get-character
                   get-file-format
                   set-file-format
                   get-overwrite-mode
                   set-overwrite-mode
                   after-insert
                   after-delete
                   can-change-style?
                   on-change-style
                   after-change-style
                   after-set-position
                   get-region-data
                   set-region-data
                   find-wordbreak
                   set-wordbreak-map
                   get-wordbreak-map
                   caret-hidden?
                   on-new-string-snip
                   on-new-tab-snip
                   set-autowrap-bitmap
                   set-wordbreak-func
                   set-clickback
                   remove-clickback
                   change-style
                   copy-self-to
                   adjust-cursor
                   size-cache-invalid
                   on-local-event
                   on-local-char
                   on-default-event
                   on-default-char
                   on-display-size
                   set-caret-owner
                   needs-update
                   get-snip-data
                   set-snip-data
                   set-modified
                   release-snip
                   set-filename
                   on-new-image-snip
                   can-save-file?
                   on-save-file
                   after-save-file
                   can-load-file?
                   on-load-file
                   after-load-file
                   on-edit-sequence
                   after-edit-sequence
                   get-flattened-text
                   get-max-width
                   get-min-width
                   set-max-width
                   set-min-width
                   get-max-height
                   get-min-height
                   set-max-height
                   set-min-height
                   read-from-file
                   write-to-file
                   style-has-changed
                   begin-edit-sequence
                   end-edit-sequence
                   refresh-delayed?
                   get-snip-location
                   num-scroll-lines
                   find-scroll-line
                   get-snip-position-and-location
                   begin-write-header-footer-to-file
                   end-write-header-footer-to-file
                   get-inactive-caret-threshold
                   set-inactive-caret-threshold
                   editor-location-to-dc-location
                   dc-location-to-editor-location
                   get-position
                   set-max-undo-history
                   get-max-undo-history
                   set-load-overwrites-styles
                   get-load-overwrites-styles
                   auto-wrap
                   get-canvas
                   add-canvas
                   get-canvases
                   get-active-canvas
                   set-active-canvas
                   remove-canvas
                   get-max-view-size))
               (define tab-snip<%>
                 (interface
                   ()
                   write
                   read
                   get-style
                   set-style
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   draw
                   split
                   merge-with
                   match?
                   next
                   previous
                   resize
                   get-text
                   insert
                   copy
                   own-caret
                   blink-caret
                   get-extent
                   get-admin
                   set-admin
                   on-event
                   on-char
                   do-edit-operation
                   set-snipclass
                   get-snipclass
                   release-from-owner
                   partial-offset
                   find-scroll-step
                   adjust-cursor
                   size-cache-invalid
                   get-num-scroll-steps
                   get-scroll-step-offset))
               (define style-list<%>
                 (interface
                   ()
                   clear
                   number
                   copy
                   basic-style
                   convert
                   find-named-style
                   new-named-style
                   replace-named-style
                   index-to-style
                   style-to-index
                   notify-on-change
                   forget-notification
                   find-or-create-style
                   find-or-create-join-style))
               (define style-delta<%>
                 (interface
                   ()
                   equal?
                   get-family
                   get-face
                   copy
                   set-transparent-text-backing-off
                   get-transparent-text-backing-off
                   set-transparent-text-backing-on
                   get-transparent-text-backing-on
                   set-face
                   set-family
                   set-delta
                   collapse
                   set-alignment-off
                   get-alignment-off
                   set-alignment-on
                   get-alignment-on
                   get-background-add
                   get-foreground-add
                   get-background-mult
                   get-foreground-mult
                   set-underlined-off
                   get-underlined-off
                   set-underlined-on
                   get-underlined-on
                   set-style-off
                   get-style-off
                   set-style-on
                   get-style-on
                   set-weight-off
                   get-weight-off
                   set-weight-on
                   get-weight-on
                   set-size-add
                   get-size-add
                   set-size-mult
                   get-size-mult
                   set-delta-face
                   set-delta-background
                   set-delta-foreground))
               (define string-snip<%>
                 (interface
                   ()
                   write
                   read
                   get-style
                   set-style
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   draw
                   split
                   merge-with
                   match?
                   next
                   previous
                   resize
                   get-text
                   insert
                   copy
                   own-caret
                   blink-caret
                   get-extent
                   get-admin
                   set-admin
                   on-event
                   on-char
                   do-edit-operation
                   set-snipclass
                   get-snipclass
                   release-from-owner
                   partial-offset
                   find-scroll-step
                   adjust-cursor
                   size-cache-invalid
                   get-num-scroll-steps
                   get-scroll-step-offset))
               (define snip-class<%>
                 (interface
                   ()
                   read
                   set-version
                   get-version
                   read-header
                   read-done
                   write-done
                   set-classname
                   get-classname
                   write-header))
               (define snip-admin<%>
                 (interface
                   ()
                   scroll-to
                   resized
                   get-editor
                   get-dc
                   get-view-size
                   set-caret-owner
                   needs-update
                   release-snip
                   get-view
                   recounted
                   update-cursor))
               (define snip<%>
                 (interface
                   ()
                   write
                   get-style
                   set-style
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   draw
                   split
                   merge-with
                   match?
                   next
                   previous
                   resize
                   get-text
                   copy
                   own-caret
                   blink-caret
                   get-extent
                   get-admin
                   set-admin
                   on-event
                   on-char
                   do-edit-operation
                   set-snipclass
                   get-snipclass
                   release-from-owner
                   partial-offset
                   find-scroll-step
                   adjust-cursor
                   size-cache-invalid
                   get-num-scroll-steps
                   get-scroll-step-offset))
               (define slider<%>
                 (interface
                   ()
                   on-drop-file
                   get-value
                   set-value
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define separator-menu-item<%>
                 (interface () delete get-parent restore is-deleted?))
               (define scroll-event<%>
                 (interface
                   ()
                   set-position
                   set-direction
                   get-direction
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp
                   get-position))
               (define radio-box<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-selection
                   set-selection
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   get-item-plain-label
                   on-subwindow-char
                   get-item-label
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   get-number
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define ps-setup<%>
                 (interface
                   ()
                   copy-from
                   get-file
                   get-command
                   get-mode
                   get-options
                   get-scaling
                   get-level-2
                   set-command
                   set-file
                   set-mode
                   set-options
                   set-scaling
                   set-level-2
                   get-preview-command
                   get-orientation
                   get-translation
                   get-paper-name
                   get-afm-path
                   set-preview-command
                   set-orientation
                   set-translation
                   set-paper-name
                   set-afm-path))
               (define printer-dc<%>
                 (interface
                   ()
                   ok?
                   get-text-extent
                   clear
                   get-size
                   end-drawing
                   draw-line
                   draw-point
                   draw-spline
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   draw-icon
                   blit
                   try-color
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   begin-drawing
                   draw-rectangle
                   draw-ellipse
                   draw-polygon
                   set-clipping-region
                   get-clipping-region
                   set-background
                   set-text-background
                   set-text-foreground
                   get-char-height
                   get-char-width
                   set-background-mode
                   set-user-scale
                   set-device-origin
                   get-background
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   draw-rounded-rectangle
                   destroy-clipping-region
                   set-logical-function
                   get-logical-function))
               (define post-script-dc<%>
                 (interface
                   ()
                   ok?
                   get-text-extent
                   clear
                   get-size
                   end-drawing
                   draw-line
                   draw-point
                   draw-spline
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   draw-icon
                   blit
                   try-color
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   begin-drawing
                   draw-rectangle
                   draw-ellipse
                   draw-polygon
                   set-clipping-region
                   get-clipping-region
                   set-background
                   set-text-background
                   set-text-foreground
                   get-char-height
                   get-char-width
                   set-background-mode
                   set-user-scale
                   set-device-origin
                   get-background
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   draw-rounded-rectangle
                   destroy-clipping-region
                   set-logical-function
                   get-logical-function))
               (define popup-menu<%> (interface () get-items))
               (define point<%> (interface () set-y set-x get-x get-y))
               (define pen-list<%> (interface () find-or-create-pen))
               (define pen<%>
                 (interface
                   ()
                   set-width
                   get-cap
                   set-cap
                   get-join
                   set-join
                   get-color
                   set-color
                   get-stipple
                   set-stipple
                   get-style
                   set-style
                   get-width))
               (define pasteboard<%>
                 (interface
                   ()
                   cut
                   raise
                   print
                   clear
                   delete
                   load-file
                   save-file
                   move
                   set-cursor
                   refresh
                   remove
                   move-to
                   resize
                   lower
                   set-before
                   set-after
                   no-selected
                   get-center
                   on-move-to
                   can-resize?
                   on-resize
                   can-select?
                   on-select
                   erase
                   do-copy
                   do-paste
                   find-snip
                   can-insert?
                   on-insert
                   can-delete?
                   on-delete
                   insert
                   copy
                   paste
                   kill
                   copy-self
                   own-caret
                   blink-caret
                   on-focus
                   on-change
                   scroll-to
                   resized
                   on-new-box
                   get-file
                   put-file
                   insert-file
                   get-extent
                   get-descent
                   get-space
                   print-to-dc
                   get-admin
                   set-admin
                   select-all
                   undo
                   redo
                   clear-undos
                   set-keymap
                   get-keymap
                   lock
                   is-locked?
                   insert-box
                   on-paint
                   on-event
                   on-char
                   get-dc
                   global-to-local
                   local-to-global
                   get-view-size
                   do-edit-operation
                   get-style-list
                   set-style-list
                   is-modified?
                   get-filename
                   insert-image
                   get-focus-snip
                   find-next-selected-snip
                   interactive-adjust-mouse
                   interactive-adjust-move
                   interactive-adjust-resize
                   can-interactive-move?
                   after-interactive-move
                   can-interactive-resize?
                   on-interactive-resize
                   after-interactive-resize
                   get-selection-visible
                   set-selection-visible
                   read-header-from-file
                   read-footer-from-file
                   write-headers-to-file
                   write-footers-to-file
                   invalidate-bitmap-cache
                   scroll-line-location
                   set-selected
                   add-selected
                   remove-selected
                   find-first-snip
                   is-selected?
                   can-move-to?
                   after-move-to
                   after-resize
                   after-select
                   on-double-click
                   on-interactive-move
                   get-dragable
                   set-dragable
                   get-scroll-step
                   set-scroll-step
                   after-insert
                   after-delete
                   change-style
                   copy-self-to
                   adjust-cursor
                   size-cache-invalid
                   on-local-event
                   on-local-char
                   on-default-event
                   on-default-char
                   on-display-size
                   set-caret-owner
                   needs-update
                   get-snip-data
                   set-snip-data
                   set-modified
                   release-snip
                   set-filename
                   on-new-image-snip
                   can-save-file?
                   on-save-file
                   after-save-file
                   can-load-file?
                   on-load-file
                   after-load-file
                   on-edit-sequence
                   after-edit-sequence
                   get-flattened-text
                   get-max-width
                   get-min-width
                   set-max-width
                   set-min-width
                   get-max-height
                   get-min-height
                   set-max-height
                   set-min-height
                   read-from-file
                   write-to-file
                   style-has-changed
                   begin-edit-sequence
                   end-edit-sequence
                   refresh-delayed?
                   get-snip-location
                   num-scroll-lines
                   find-scroll-line
                   begin-write-header-footer-to-file
                   end-write-header-footer-to-file
                   get-inactive-caret-threshold
                   set-inactive-caret-threshold
                   editor-location-to-dc-location
                   dc-location-to-editor-location
                   set-max-undo-history
                   get-max-undo-history
                   set-load-overwrites-styles
                   get-load-overwrites-styles
                   auto-wrap
                   get-canvas
                   add-canvas
                   get-canvases
                   get-active-canvas
                   set-active-canvas
                   remove-canvas
                   get-max-view-size))
               (define panel<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   on-focus
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   get-client-size
                   focus
                   is-enabled?
                   border
                   get-alignment
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define pane<%>
                 (interface
                   ()
                   get-parent
                   border
                   get-alignment
                   get-top-level-window
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child))
               (define mouse-event<%>
                 (interface
                   ()
                   set-y
                   set-x
                   get-x
                   get-y
                   button-up?
                   dragging?
                   entering?
                   leaving?
                   moving?
                   set-right-down
                   get-right-down
                   set-middle-down
                   get-middle-down
                   set-left-down
                   get-left-down
                   button-changed?
                   button-down?
                   set-alt-down
                   get-alt-down
                   set-meta-down
                   get-meta-down
                   set-control-down
                   get-control-down
                   set-shift-down
                   get-shift-down
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp))
               (define meta-file-dc<%>
                 (interface
                   ()
                   ok?
                   get-text-extent
                   clear
                   get-size
                   end-drawing
                   draw-line
                   draw-point
                   draw-spline
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   draw-icon
                   blit
                   try-color
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   begin-drawing
                   draw-rectangle
                   draw-ellipse
                   draw-polygon
                   set-clipping-region
                   get-clipping-region
                   set-background
                   set-text-background
                   set-text-foreground
                   get-char-height
                   get-char-width
                   set-background-mode
                   set-user-scale
                   set-device-origin
                   get-background
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   draw-rounded-rectangle
                   destroy-clipping-region
                   set-logical-function
                   get-logical-function
                   close))
               (define message<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define menu-bar<%>
                 (interface () enable is-enabled? get-frame get-items))
               (define menu<%> (interface () get-item get-items))
               (define memory-dc<%>
                 (interface
                   ()
                   ok?
                   get-text-extent
                   clear
                   get-size
                   get-pixel
                   set-pixel
                   end-drawing
                   draw-line
                   draw-point
                   draw-spline
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   draw-icon
                   blit
                   try-color
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   select-object
                   begin-set-pixel
                   end-set-pixel
                   begin-drawing
                   draw-rectangle
                   draw-ellipse
                   draw-polygon
                   set-clipping-region
                   get-clipping-region
                   set-background
                   set-text-background
                   set-text-foreground
                   get-char-height
                   get-char-width
                   set-background-mode
                   set-user-scale
                   set-device-origin
                   get-background
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   draw-rounded-rectangle
                   destroy-clipping-region
                   set-logical-function
                   get-logical-function))
               (define list-box<%>
                 (interface
                   ()
                   set
                   append
                   on-drop-file
                   clear
                   delete
                   get-data
                   set-data
                   set-string
                   find-string
                   get-string
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   is-selected?
                   get-selections
                   get-selection
                   set-selection
                   get-client-size
                   number-of-visible-items
                   set-first-visible-item
                   get-string-selection
                   set-string-selection
                   focus
                   select
                   is-enabled?
                   get-top-level-window
                   get-first-visible-item
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   get-number
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define keymap<%>
                 (interface
                   ()
                   get-double-click-interval
                   set-double-click-interval
                   set-grab-key-function
                   remove-grab-key-function
                   set-grab-mouse-function
                   remove-grab-mouse-function
                   set-break-sequence-callback
                   remove-chained-keymap
                   handle-key-event
                   handle-mouse-event
                   break-sequence
                   map-function
                   implies-shift
                   add-key-function
                   add-mouse-function
                   call-function
                   set-error-callback
                   chain-to-keymap))
               (define key-event<%>
                 (interface
                   ()
                   set-y
                   set-x
                   get-x
                   get-y
                   set-alt-down
                   get-alt-down
                   set-meta-down
                   get-meta-down
                   set-control-down
                   get-control-down
                   set-shift-down
                   get-shift-down
                   set-key-code
                   get-key-code
                   set-time-stamp
                   get-time-stamp))
               (define image-snip<%>
                 (interface
                   ()
                   write
                   load-file
                   get-style
                   set-style
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   draw
                   split
                   merge-with
                   match?
                   next
                   previous
                   resize
                   get-text
                   copy
                   own-caret
                   blink-caret
                   get-extent
                   get-admin
                   set-admin
                   on-event
                   on-char
                   do-edit-operation
                   get-filename
                   get-filetype
                   set-snipclass
                   get-snipclass
                   release-from-owner
                   partial-offset
                   find-scroll-step
                   adjust-cursor
                   size-cache-invalid
                   get-num-scroll-steps
                   get-scroll-step-offset
                   set-bitmap
                   set-offset))
               (define icon<%>
                 (interface
                   ()
                   ok?
                   get-depth
                   load-file
                   save-file
                   get-height
                   get-width))
               (define horizontal-panel<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   on-focus
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   get-client-size
                   focus
                   is-enabled?
                   border
                   get-alignment
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define horizontal-pane<%>
                 (interface
                   ()
                   get-parent
                   border
                   get-alignment
                   get-top-level-window
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   vert-margin
                   spacing
                   add-child
                   horiz-margin
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child))
               (define gauge<%>
                 (interface
                   ()
                   on-drop-file
                   set-range
                   get-range
                   get-value
                   set-value
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define frame<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   iconize
                   set-icon
                   maximize
                   on-activate
                   on-close
                   move
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   center
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   resize
                   on-focus
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   get-menu-bar
                   set-status-text
                   create-status-line
                   get-client-size
                   focus
                   is-enabled?
                   border
                   get-alignment
                   get-top-level-window
                   get-edit-target-window
                   get-edit-target-object
                   get-eventspace
                   get-focus-window
                   get-focus-object
                   on-subwindow-char
                   has-status-line?
                   is-iconized?
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   can-close?
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define font-list<%> (interface () find-or-create-font))
               (define font<%>
                 (interface
                   ()
                   get-family
                   get-face
                   get-weight
                   get-font-id
                   get-style
                   get-point-size
                   get-underlined))
               (define event<%> (interface () set-time-stamp get-time-stamp))
               (define editor-wordbreak-map<%> (interface () set-map get-map))
               (define editor-stream-out-string-base<%>
                 (interface () write get-string tell seek bad?))
               (define editor-stream-out-base<%>
                 (interface () write tell seek bad?))
               (define editor-stream-out<%>
                 (interface () put << ok? put-fixed jump-to tell))
               (define editor-stream-in-string-base<%>
                 (interface () read tell seek skip bad?))
               (define editor-stream-in-base<%>
                 (interface () read tell seek skip bad?))
               (define editor-stream-in<%>
                 (interface
                   ()
                   get
                   >>
                   ok?
                   get-string
                   get-fixed
                   get-exact
                   get-inexact
                   jump-to
                   tell
                   skip
                   set-boundary
                   remove-boundary))
               (define editor-snip<%>
                 (interface
                   ()
                   write
                   get-style
                   set-style
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   draw
                   split
                   merge-with
                   match?
                   next
                   previous
                   resize
                   get-text
                   copy
                   own-caret
                   blink-caret
                   get-extent
                   get-admin
                   set-admin
                   set-editor
                   get-editor
                   on-event
                   on-char
                   do-edit-operation
                   border-visible?
                   set-snipclass
                   get-snipclass
                   release-from-owner
                   partial-offset
                   find-scroll-step
                   adjust-cursor
                   size-cache-invalid
                   get-max-width
                   get-min-width
                   set-max-width
                   set-min-width
                   get-max-height
                   get-min-height
                   set-max-height
                   set-min-height
                   get-num-scroll-steps
                   get-scroll-step-offset
                   show-border
                   set-margin
                   get-margin
                   set-inset
                   get-inset))
               (define editor-data-class-list<%>
                 (interface () add nth number find find-position))
               (define editor-data-class<%>
                 (interface () read set-classname get-classname))
               (define editor-data<%>
                 (interface
                   ()
                   write
                   get-next
                   set-next
                   set-dataclass
                   get-dataclass))
               (define editor-canvas<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   on-focus
                   set-editor
                   get-editor
                   on-paint
                   on-event
                   on-char
                   popup-menu
                   get-dc
                   on-scroll
                   force-display-focus
                   warp-pointer
                   get-client-size
                   allow-scroll-to-last
                   scroll-with-bottom-base
                   call-as-primary-owner
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   min-client-width
                   min-client-height
                   lazy-refresh
                   set-line-count
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define editor-admin<%>
                 (interface
                   ()
                   scroll-to
                   resized
                   get-dc
                   needs-update
                   refresh-delayed?
                   grab-caret
                   get-view
                   get-max-view
                   update-cursor))
               (define dialog<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   on-activate
                   on-close
                   move
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   center
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   get-label
                   resize
                   on-focus
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   get-client-size
                   focus
                   is-enabled?
                   border
                   get-alignment
                   get-top-level-window
                   get-edit-target-window
                   get-edit-target-object
                   get-eventspace
                   get-focus-window
                   get-focus-object
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   set-alignment
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   can-close?
                   get-children
                   change-children
                   container-size
                   place-children
                   delete-child
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define cursor<%> (interface () ok?))
               (define control-event<%>
                 (interface
                   ()
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp))
               (define color<%>
                 (interface () ok? set red copy-from green blue))
               (define clipboard-client<%>
                 (interface () get-data add-type get-types being-replaced))
               (define choice<%>
                 (interface
                   ()
                   append
                   on-drop-file
                   clear
                   find-string
                   get-string
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-selection
                   set-selection
                   get-client-size
                   get-string-selection
                   set-string-selection
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   get-number
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define checkable-menu-item<%>
                 (interface
                   ()
                   go
                   delete
                   set-label
                   enable
                   get-parent
                   check
                   get-label
                   set-help-string
                   is-enabled?
                   get-x-shortcut-prefix
                   set-x-shortcut-prefix
                   get-plain-label
                   get-help-string
                   set-shortcut
                   get-shortcut
                   restore
                   is-deleted?
                   is-checked?))
               (define check-box<%>
                 (interface
                   ()
                   on-drop-file
                   get-value
                   set-value
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define canvas-dc<%>
                 (interface
                   ()
                   ok?
                   get-text-extent
                   clear
                   get-size
                   get-pixel
                   set-pixel
                   end-drawing
                   draw-line
                   draw-point
                   draw-spline
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   draw-icon
                   blit
                   try-color
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   begin-set-pixel
                   end-set-pixel
                   begin-drawing
                   draw-rectangle
                   draw-ellipse
                   draw-polygon
                   set-clipping-region
                   get-clipping-region
                   set-background
                   set-text-background
                   set-text-foreground
                   get-char-height
                   get-char-width
                   set-background-mode
                   set-user-scale
                   set-device-origin
                   get-background
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   draw-rounded-rectangle
                   destroy-clipping-region
                   set-logical-function
                   get-logical-function))
               (define button<%>
                 (interface
                   ()
                   on-drop-file
                   set-label
                   set-cursor
                   show
                   is-shown?
                   get-size
                   enable
                   refresh
                   get-parent
                   get-height
                   get-width
                   get-x
                   get-y
                   on-size
                   command
                   get-label
                   on-focus
                   get-client-size
                   focus
                   is-enabled?
                   get-top-level-window
                   on-subwindow-char
                   get-plain-label
                   stretchable-width
                   stretchable-height
                   min-width
                   min-height
                   vert-margin
                   on-move
                   has-focus?
                   get-cursor
                   horiz-margin
                   on-subwindow-event
                   accept-drop-files
                   client->screen
                   screen->client))
               (define brush-list<%> (interface () find-or-create-brush))
               (define brush<%>
                 (interface
                   ()
                   get-color
                   set-color
                   get-stipple
                   set-stipple
                   get-style
                   set-style))
               (define bitmap<%>
                 (interface
                   ()
                   ok?
                   get-depth
                   load-file
                   save-file
                   get-height
                   get-width))
               (define -vertical-panel%
                 (class*
                   vertical-panel%
                   (vertical-panel<%>)
                   args
                   (sequence (apply super-init args))))
               (define -vertical-pane%
                 (class*
                   vertical-pane%
                   (vertical-pane<%>)
                   args
                   (sequence (apply super-init args))))
               (define -timer%
                 (class*
                   timer%
                   (timer<%>)
                   args
                   (sequence (apply super-init args))))
               (define -text-field%
                 (class*
                   text-field%
                   (text-field<%>)
                   args
                   (sequence (apply super-init args))))
               (define -text%
                 (class*
                   text%
                   (text<%>)
                   args
                   (sequence (apply super-init args))))
               (define -tab-snip%
                 (class*
                   tab-snip%
                   (tab-snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -style-list%
                 (class*
                   style-list%
                   (style-list<%>)
                   args
                   (sequence (apply super-init args))))
               (define -style-delta%
                 (class*
                   style-delta%
                   (style-delta<%>)
                   args
                   (sequence (apply super-init args))))
               (define -string-snip%
                 (class*
                   string-snip%
                   (string-snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -snip-class%
                 (class*
                   snip-class%
                   (snip-class<%>)
                   args
                   (sequence (apply super-init args))))
               (define -snip-admin%
                 (class*
                   snip-admin%
                   (snip-admin<%>)
                   args
                   (sequence (apply super-init args))))
               (define -snip%
                 (class*
                   snip%
                   (snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -slider%
                 (class*
                   slider%
                   (slider<%>)
                   args
                   (sequence (apply super-init args))))
               (define -separator-menu-item%
                 (class*
                   separator-menu-item%
                   (separator-menu-item<%>)
                   args
                   (sequence (apply super-init args))))
               (define -scroll-event%
                 (class*
                   scroll-event%
                   (scroll-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -radio-box%
                 (class*
                   radio-box%
                   (radio-box<%>)
                   args
                   (sequence (apply super-init args))))
               (define -ps-setup%
                 (class*
                   ps-setup%
                   (ps-setup<%>)
                   args
                   (sequence (apply super-init args))))
               (define -printer-dc%
                 (class*
                   printer-dc%
                   (printer-dc<%>)
                   args
                   (sequence (apply super-init args))))
               (define -post-script-dc%
                 (class*
                   post-script-dc%
                   (post-script-dc<%>)
                   args
                   (sequence (apply super-init args))))
               (define -popup-menu%
                 (class*
                   popup-menu%
                   (popup-menu<%>)
                   args
                   (sequence (apply super-init args))))
               (define -point%
                 (class*
                   point%
                   (point<%>)
                   args
                   (sequence (apply super-init args))))
               (define -pen-list%
                 (class*
                   pen-list%
                   (pen-list<%>)
                   args
                   (sequence (apply super-init args))))
               (define -pen%
                 (class*
                   pen%
                   (pen<%>)
                   args
                   (sequence (apply super-init args))))
               (define -pasteboard%
                 (class*
                   pasteboard%
                   (pasteboard<%>)
                   args
                   (sequence (apply super-init args))))
               (define -panel%
                 (class*
                   panel%
                   (panel<%>)
                   args
                   (sequence (apply super-init args))))
               (define -pane%
                 (class*
                   pane%
                   (pane<%>)
                   args
                   (sequence (apply super-init args))))
               (define -mouse-event%
                 (class*
                   mouse-event%
                   (mouse-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -meta-file-dc%
                 (class*
                   meta-file-dc%
                   (meta-file-dc<%>)
                   args
                   (sequence (apply super-init args))))
               (define -message%
                 (class*
                   message%
                   (message<%>)
                   args
                   (sequence (apply super-init args))))
               (define -menu-bar%
                 (class*
                   menu-bar%
                   (menu-bar<%>)
                   args
                   (sequence (apply super-init args))))
               (define -menu%
                 (class*
                   menu%
                   (menu<%>)
                   args
                   (sequence (apply super-init args))))
               (define -memory-dc%
                 (class*
                   memory-dc%
                   (memory-dc<%>)
                   args
                   (sequence (apply super-init args))))
               (define -list-box%
                 (class*
                   list-box%
                   (list-box<%>)
                   args
                   (sequence (apply super-init args))))
               (define -keymap%
                 (class*
                   keymap%
                   (keymap<%>)
                   args
                   (sequence (apply super-init args))))
               (define -key-event%
                 (class*
                   key-event%
                   (key-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -image-snip%
                 (class*
                   image-snip%
                   (image-snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -icon%
                 (class*
                   icon%
                   (icon<%>)
                   args
                   (sequence (apply super-init args))))
               (define -horizontal-panel%
                 (class*
                   horizontal-panel%
                   (horizontal-panel<%>)
                   args
                   (sequence (apply super-init args))))
               (define -horizontal-pane%
                 (class*
                   horizontal-pane%
                   (horizontal-pane<%>)
                   args
                   (sequence (apply super-init args))))
               (define -gauge%
                 (class*
                   gauge%
                   (gauge<%>)
                   args
                   (sequence (apply super-init args))))
               (define -frame%
                 (class*
                   frame%
                   (frame<%>)
                   args
                   (sequence (apply super-init args))))
               (define -font-list%
                 (class*
                   font-list%
                   (font-list<%>)
                   args
                   (sequence (apply super-init args))))
               (define -font%
                 (class*
                   font%
                   (font<%>)
                   args
                   (sequence (apply super-init args))))
               (define -event%
                 (class*
                   event%
                   (event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-wordbreak-map%
                 (class*
                   editor-wordbreak-map%
                   (editor-wordbreak-map<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-out-string-base%
                 (class*
                   editor-stream-out-string-base%
                   (editor-stream-out-string-base<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-out-base%
                 (class*
                   editor-stream-out-base%
                   (editor-stream-out-base<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-out%
                 (class*
                   editor-stream-out%
                   (editor-stream-out<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-in-string-base%
                 (class*
                   editor-stream-in-string-base%
                   (editor-stream-in-string-base<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-in-base%
                 (class*
                   editor-stream-in-base%
                   (editor-stream-in-base<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-stream-in%
                 (class*
                   editor-stream-in%
                   (editor-stream-in<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-snip%
                 (class*
                   editor-snip%
                   (editor-snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-data-class-list%
                 (class*
                   editor-data-class-list%
                   (editor-data-class-list<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-data-class%
                 (class*
                   editor-data-class%
                   (editor-data-class<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-data%
                 (class*
                   editor-data%
                   (editor-data<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-canvas%
                 (class*
                   editor-canvas%
                   (editor-canvas<%>)
                   args
                   (sequence (apply super-init args))))
               (define -editor-admin%
                 (class*
                   editor-admin%
                   (editor-admin<%>)
                   args
                   (sequence (apply super-init args))))
               (define -dialog%
                 (class*
                   dialog%
                   (dialog<%>)
                   args
                   (sequence (apply super-init args))))
               (define -cursor%
                 (class*
                   cursor%
                   (cursor<%>)
                   args
                   (sequence (apply super-init args))))
               (define -control-event%
                 (class*
                   control-event%
                   (control-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -color%
                 (class*
                   color%
                   (color<%>)
                   args
                   (sequence (apply super-init args))))
               (define -clipboard-client%
                 (class*
                   clipboard-client%
                   (clipboard-client<%>)
                   args
                   (sequence (apply super-init args))))
               (define -choice%
                 (class*
                   choice%
                   (choice<%>)
                   args
                   (sequence (apply super-init args))))
               (define -checkable-menu-item%
                 (class*
                   checkable-menu-item%
                   (checkable-menu-item<%>)
                   args
                   (sequence (apply super-init args))))
               (define -check-box%
                 (class*
                   check-box%
                   (check-box<%>)
                   args
                   (sequence (apply super-init args))))
               (define -canvas-dc%
                 (class*
                   canvas-dc%
                   (canvas-dc<%>)
                   args
                   (sequence (apply super-init args))))
               (define -button%
                 (class*
                   button%
                   (button<%>)
                   args
                   (sequence (apply super-init args))))
               (define -brush-list%
                 (class*
                   brush-list%
                   (brush-list<%>)
                   args
                   (sequence (apply super-init args))))
               (define -brush%
                 (class*
                   brush%
                   (brush<%>)
                   args
                   (sequence (apply super-init args))))
               (define -bitmap%
                 (class*
                   bitmap%
                   (bitmap<%>)
                   args
                   (sequence (apply super-init args)))))
             mred)))
    (export (open
             (mred :
                   (yield write-resource
                          write-editor-global-header
                          write-editor-global-footer
                          window<%>
                          unregister-collecting-blit
                          top-level-window<%>
                          the-style-list
                          the-pen-list
                          the-font-name-directory
                          the-font-list
                          the-editor-wordbreak-map
                          the-color-database
                          the-clipboard
                          the-brush-list
                          subwindow<%>
                          submenu-item<%>
                          subarea<%>
                          style%
                          special-control-key
                          snip-class-list%
                          shortcut-menu-item<%>
                          set-editor-print-margin
                          register-collecting-blit
                          read-editor-global-header
                          read-editor-global-footer
                          queue-callback
                          put-file
                          play-sound
                          mult-color<%>
                          mred@
                          meta-file<%>
                          message-box
                          menu-item<%>
                          menu-item-container<%>
                          menu-item%
                          make-meta-file-placeable
                          make-eventspace
                          list-control<%>
                          labelled-menu-item<%>
                          label->plain-label
                          is-color-display?
                          is-busy?
                          graphical-read-eval-print-loop
                          get-top-level-windows
                          get-top-level-focus-window
                          get-top-level-edit-target-window
                          get-the-snip-class-list
                          get-the-editor-data-class-list
                          get-text-from-user
                          get-resource
                          get-ps-setup-from-user
                          get-font-from-user
                          get-file
                          get-face-list
                          get-editor-print-margin
                          get-display-size
                          get-display-depth
                          get-color-from-user
                          get-choice-from-user
                          font-name-directory<%>
                          flush-display
                          find-graphical-system-path
                          eventspace?
                          eventspace-parameterization
                          event-dispatch-handler
                          end-busy-cursor
                          editor<%>
                          editor-snip-editor-admin%
                          editor-set-x-selection-mode
                          dc<%>
                          current-ps-setup
                          current-eventspace
                          control<%>
                          color-database<%>
                          clipboard<%>
                          check-for-break
                          canvas<%>
                          canvas%
                          bell
                          begin-busy-cursor
                          area<%>
                          area-container<%>
                          area-container-window<%>
                          append-editor-operation-menu-items
                          append-editor-font-menu-items
                          add-text-keymap-functions
                          add-pasteboard-keymap-functions
                          add-editor-keymap-functions
                          add-color<%>)))
            (open interfaces))))
