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
   region<%>
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
   message<%>
   menu-bar<%>
   menu<%>
   list-box<%>
   keymap<%>
   key-event<%>
   image-snip<%>
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
   button<%>
   brush-list<%>
   brush<%>
   bitmap-dc<%>
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
              region<%>
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
              message<%>
              menu-bar<%>
              menu<%>
              list-box<%>
              keymap<%>
              key-event<%>
              image-snip<%>
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
              button<%>
              brush-list<%>
              brush<%>
              bitmap-dc<%>
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
              region%
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
              message%
              menu-bar%
              menu%
              list-box%
              keymap%
              key-event%
              image-snip%
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
              button%
              brush-list%
              brush%
              bitmap-dc%
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
                 region<%>
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
                 message<%>
                 menu-bar<%>
                 menu<%>
                 list-box<%>
                 keymap<%>
                 key-event<%>
                 image-snip<%>
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
                 button<%>
                 brush-list<%>
                 brush<%>
                 bitmap-dc<%>
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
                 region%
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
                 message%
                 menu-bar%
                 menu%
                 list-box%
                 keymap%
                 key-event%
                 image-snip%
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
                 button%
                 brush-list%
                 brush%
                 bitmap-dc%
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
                 (-region% region%)
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
                 (-message% message%)
                 (-menu-bar% menu-bar%)
                 (-menu% menu%)
                 (-list-box% list-box%)
                 (-keymap% keymap%)
                 (-key-event% key-event%)
                 (-image-snip% image-snip%)
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
                 (-button% button%)
                 (-brush-list% brush-list%)
                 (-brush% brush%)
                 (-bitmap-dc% bitmap-dc%)
                 (-bitmap% bitmap%))
               (define vertical-panel<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   set-alignment
                   get-alignment
                   focus
                   border
                   get-top-level-window
                   change-children
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
                   container-size
                   place-children
                   delete-child
                   client->screen
                   screen->client))
               (define vertical-pane<%>
                 (interface
                   ()
                   get-parent
                   min-width
                   min-height
                   spacing
                   add-child
                   set-alignment
                   get-alignment
                   border
                   get-top-level-window
                   change-children
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
                   container-size
                   place-children
                   delete-child))
               (define timer<%> (interface () interval notify start stop))
               (define text-field<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-editor
                   get-value
                   set-value
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define text<%>
                 (interface
                   ()
                   erase
                   insert
                   cut
                   copy
                   paste
                   kill
                   undo
                   redo
                   lock
                   clear
                   delete
                   get-dc
                   print
                   find-string
                   get-position
                   get-admin
                   set-admin
                   select-all
                   set-keymap
                   get-keymap
                   is-locked?
                   insert-box
                   on-paint
                   on-event
                   on-char
                   load-file
                   save-file
                   set-cursor
                   refresh
                   after-edit-sequence
                   begin-edit-sequence
                   scroll-line-location
                   set-max-undo-history
                   get-max-undo-history
                   get-snip-position-and-location
                   begin-write-header-footer-to-file
                   end-write-header-footer-to-file
                   get-inactive-caret-threshold
                   set-inactive-caret-threshold
                   editor-location-to-dc-location
                   dc-location-to-editor-location
                   position-location
                   line-end-position
                   position-paragraph
                   paragraph-end-line
                   find-string-all
                   get-snip-position
                   get-file-format
                   set-file-format
                   get-overwrite-mode
                   set-overwrite-mode
                   can-change-style?
                   on-change-style
                   after-change-style
                   after-set-position
                   get-region-data
                   set-region-data
                   set-wordbreak-map
                   get-wordbreak-map
                   on-new-string-snip
                   on-new-tab-snip
                   set-wordbreak-func
                   remove-clickback
                   size-cache-invalid
                   find-first-snip
                   on-default-event
                   on-default-char
                   on-display-size
                   set-caret-owner
                   on-new-image-snip
                   after-save-file
                   after-load-file
                   on-edit-sequence
                   get-flattened-text
                   style-has-changed
                   end-edit-sequence
                   refresh-delayed?
                   get-snip-location
                   num-scroll-lines
                   find-scroll-line
                   global-to-local
                   local-to-global
                   do-edit-operation
                   move-position
                   find-position
                   position-line
                   line-location
                   line-length
                   last-position
                   line-paragraph
                   last-paragraph
                   get-character
                   can-insert?
                   after-insert
                   can-delete?
                   after-delete
                   find-wordbreak
                   caret-hidden?
                   set-clickback
                   change-style
                   copy-self-to
                   adjust-cursor
                   blink-caret
                   on-local-event
                   on-local-char
                   needs-update
                   get-snip-data
                   set-snip-data
                   set-modified
                   release-snip
                   set-filename
                   can-save-file?
                   on-save-file
                   can-load-file?
                   on-load-file
                   insert-file
                   get-descent
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
                   print-to-dc
                   get-view-size
                   clear-undos
                   get-style-list
                   set-style-list
                   is-modified?
                   get-filename
                   insert-image
                   get-focus-snip
                   set-position
                   set-position-bias-scroll
                   get-visible-position-range
                   paragraph-start-position
                   can-set-size-constraint?
                   after-set-size-constraint
                   invalidate-bitmap-cache
                   set-load-overwrites-styles
                   get-load-overwrites-styles
                   auto-wrap
                   set-anchor
                   get-anchor
                   flash-on
                   flash-off
                   paste-next
                   do-copy
                   do-paste
                   split-snip
                   find-line
                   last-line
                   find-snip
                   get-text
                   get-tabs
                   set-tabs
                   on-insert
                   on-delete
                   hide-caret
                   copy-self
                   own-caret
                   on-focus
                   on-change
                   scroll-to
                   resized
                   on-new-box
                   get-file
                   put-file
                   get-extent
                   get-space
                   get-visible-line-range
                   find-position-in-line
                   get-between-threshold
                   set-between-threshold
                   line-start-position
                   paragraph-end-position
                   paragraph-start-line
                   on-set-size-constraint
                   set-autowrap-bitmap
                   read-header-from-file
                   read-footer-from-file
                   write-headers-to-file
                   write-footers-to-file
                   get-start-position
                   get-end-position
                   scroll-to-position
                   get-active-canvas
                   set-active-canvas
                   get-max-view-size
                   get-canvases
                   remove-canvas
                   get-canvas
                   add-canvas))
               (define tab-snip<%>
                 (interface
                   ()
                   draw
                   split
                   match?
                   next
                   resize
                   insert
                   copy
                   write
                   read
                   get-admin
                   set-admin
                   on-event
                   on-char
                   get-style
                   set-style
                   size-cache-invalid
                   do-edit-operation
                   adjust-cursor
                   blink-caret
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   merge-with
                   previous
                   get-text
                   own-caret
                   get-extent
                   get-num-scroll-steps
                   get-scroll-step-offset
                   release-from-owner
                   find-scroll-step
                   set-snipclass
                   get-snipclass
                   partial-offset))
               (define style-list<%>
                 (interface
                   ()
                   copy
                   clear
                   number
                   convert
                   find-named-style
                   new-named-style
                   notify-on-change
                   basic-style
                   index-to-style
                   style-to-index
                   find-or-create-style
                   replace-named-style
                   forget-notification
                   find-or-create-join-style))
               (define style-delta<%>
                 (interface
                   ()
                   copy
                   equal?
                   get-family
                   get-face
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
                   get-background-mult
                   get-foreground-mult
                   set-delta-background
                   set-delta-foreground))
               (define string-snip<%>
                 (interface
                   ()
                   draw
                   split
                   match?
                   next
                   resize
                   insert
                   copy
                   write
                   read
                   get-admin
                   set-admin
                   on-event
                   on-char
                   get-style
                   set-style
                   size-cache-invalid
                   do-edit-operation
                   adjust-cursor
                   blink-caret
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   merge-with
                   previous
                   get-text
                   own-caret
                   get-extent
                   get-num-scroll-steps
                   get-scroll-step-offset
                   release-from-owner
                   find-scroll-step
                   set-snipclass
                   get-snipclass
                   partial-offset))
               (define snip-class<%>
                 (interface
                   ()
                   read
                   read-done
                   write-done
                   set-version
                   get-version
                   set-classname
                   get-classname
                   read-header
                   write-header))
               (define snip-admin<%>
                 (interface
                   ()
                   get-dc
                   get-editor
                   set-caret-owner
                   needs-update
                   release-snip
                   get-view-size
                   get-view
                   recounted
                   scroll-to
                   resized
                   update-cursor))
               (define snip<%>
                 (interface
                   ()
                   draw
                   split
                   match?
                   next
                   resize
                   copy
                   write
                   get-admin
                   set-admin
                   on-event
                   on-char
                   get-style
                   set-style
                   size-cache-invalid
                   do-edit-operation
                   adjust-cursor
                   blink-caret
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   merge-with
                   previous
                   get-text
                   own-caret
                   get-extent
                   get-num-scroll-steps
                   get-scroll-step-offset
                   release-from-owner
                   find-scroll-step
                   set-snipclass
                   get-snipclass
                   partial-offset))
               (define slider<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-value
                   set-value
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define separator-menu-item<%>
                 (interface () delete get-parent restore is-deleted?))
               (define scroll-event<%>
                 (interface
                   ()
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp
                   get-position
                   set-position
                   set-direction
                   get-direction))
               (define region<%>
                 (interface
                   ()
                   get-dc
                   union
                   set-rectangle
                   set-ellipse
                   set-polygon
                   get-bounding-box
                   set-arc
                   intersect
                   subtract
                   is-empty?
                   set-rounded-rectangle))
               (define radio-box<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   get-selection
                   set-selection
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   get-number
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   get-item-label
                   focus
                   get-top-level-window
                   get-item-plain-label
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define ps-setup<%>
                 (interface
                   ()
                   copy-from
                   get-mode
                   set-file
                   set-mode
                   get-file
                   get-command
                   get-options
                   get-scaling
                   get-paper-name
                   get-afm-path
                   get-level-2
                   set-command
                   set-options
                   set-scaling
                   set-paper-name
                   set-afm-path
                   set-level-2
                   get-preview-command
                   set-preview-command
                   get-orientation
                   get-translation
                   set-orientation
                   set-translation))
               (define printer-dc<%>
                 (interface
                   ()
                   clear
                   ok?
                   get-text-extent
                   draw-line
                   draw-point
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   try-color
                   set-scale
                   set-origin
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   get-size
                   draw-rounded-rectangle
                   set-clipping-region
                   get-clipping-region
                   set-text-background
                   set-text-foreground
                   draw-bitmap-section
                   set-background-mode
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   set-clipping-rect
                   get-char-height
                   begin-drawing
                   end-drawing
                   draw-rectangle
                   draw-spline
                   draw-ellipse
                   draw-polygon
                   set-background
                   get-char-width
                   draw-bitmap
                   get-background))
               (define post-script-dc<%>
                 (interface
                   ()
                   clear
                   ok?
                   get-text-extent
                   draw-line
                   draw-point
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   try-color
                   set-scale
                   set-origin
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   get-size
                   draw-rounded-rectangle
                   set-clipping-region
                   get-clipping-region
                   set-text-background
                   set-text-foreground
                   draw-bitmap-section
                   set-background-mode
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   set-clipping-rect
                   get-char-height
                   begin-drawing
                   end-drawing
                   draw-rectangle
                   draw-spline
                   draw-ellipse
                   draw-polygon
                   set-background
                   get-char-width
                   draw-bitmap
                   get-background))
               (define popup-menu<%> (interface () get-items))
               (define point<%> (interface () set-y set-x get-x get-y))
               (define pen-list<%> (interface () find-or-create-pen))
               (define pen<%>
                 (interface
                   ()
                   get-stipple
                   set-stipple
                   set-width
                   get-cap
                   set-cap
                   get-join
                   set-join
                   get-color
                   set-color
                   get-style
                   set-style
                   get-width))
               (define pasteboard<%>
                 (interface
                   ()
                   remove
                   resize
                   lower
                   erase
                   insert
                   cut
                   copy
                   paste
                   kill
                   undo
                   redo
                   lock
                   clear
                   delete
                   get-dc
                   move
                   raise
                   print
                   get-admin
                   set-admin
                   select-all
                   set-keymap
                   get-keymap
                   is-locked?
                   insert-box
                   on-paint
                   on-event
                   on-char
                   load-file
                   save-file
                   set-cursor
                   refresh
                   after-edit-sequence
                   begin-edit-sequence
                   scroll-line-location
                   set-max-undo-history
                   get-max-undo-history
                   begin-write-header-footer-to-file
                   end-write-header-footer-to-file
                   get-inactive-caret-threshold
                   set-inactive-caret-threshold
                   editor-location-to-dc-location
                   dc-location-to-editor-location
                   size-cache-invalid
                   find-first-snip
                   on-default-event
                   on-default-char
                   on-display-size
                   set-caret-owner
                   on-new-image-snip
                   after-save-file
                   after-load-file
                   on-edit-sequence
                   get-flattened-text
                   style-has-changed
                   end-edit-sequence
                   refresh-delayed?
                   get-snip-location
                   num-scroll-lines
                   find-scroll-line
                   global-to-local
                   local-to-global
                   do-edit-operation
                   set-selected
                   add-selected
                   no-selected
                   is-selected?
                   can-move-to?
                   after-move-to
                   can-resize?
                   after-resize
                   can-select?
                   after-select
                   get-dragable
                   set-dragable
                   can-insert?
                   after-insert
                   can-delete?
                   after-delete
                   change-style
                   copy-self-to
                   adjust-cursor
                   blink-caret
                   on-local-event
                   on-local-char
                   needs-update
                   get-snip-data
                   set-snip-data
                   set-modified
                   release-snip
                   set-filename
                   can-save-file?
                   on-save-file
                   can-load-file?
                   on-load-file
                   insert-file
                   get-descent
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
                   print-to-dc
                   get-view-size
                   clear-undos
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
                   can-interactive-resize?
                   after-interactive-resize
                   invalidate-bitmap-cache
                   set-load-overwrites-styles
                   get-load-overwrites-styles
                   auto-wrap
                   move-to
                   set-before
                   set-after
                   get-center
                   on-move-to
                   on-resize
                   on-select
                   do-copy
                   do-paste
                   find-snip
                   on-insert
                   on-delete
                   copy-self
                   own-caret
                   on-focus
                   on-change
                   scroll-to
                   resized
                   on-new-box
                   get-file
                   put-file
                   get-extent
                   get-space
                   can-interactive-move?
                   on-interactive-move
                   after-interactive-move
                   on-interactive-resize
                   get-selection-visible
                   set-selection-visible
                   read-header-from-file
                   read-footer-from-file
                   write-headers-to-file
                   write-footers-to-file
                   remove-selected
                   on-double-click
                   get-scroll-step
                   set-scroll-step
                   get-active-canvas
                   set-active-canvas
                   get-max-view-size
                   get-canvases
                   remove-canvas
                   get-canvas
                   add-canvas))
               (define panel<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   set-alignment
                   get-alignment
                   focus
                   border
                   get-top-level-window
                   change-children
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
                   container-size
                   place-children
                   delete-child
                   client->screen
                   screen->client))
               (define pane<%>
                 (interface
                   ()
                   get-parent
                   min-width
                   min-height
                   spacing
                   add-child
                   set-alignment
                   get-alignment
                   border
                   get-top-level-window
                   change-children
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
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
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp
                   button-up?
                   dragging?
                   entering?
                   leaving?
                   moving?
                   set-middle-down
                   get-middle-down
                   button-changed?
                   set-control-down
                   get-control-down
                   set-right-down
                   get-right-down
                   set-left-down
                   get-left-down
                   button-down?
                   set-alt-down
                   get-alt-down
                   set-meta-down
                   get-meta-down
                   set-shift-down
                   get-shift-down))
               (define message<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define menu-bar<%>
                 (interface () enable get-items get-frame is-enabled?))
               (define menu<%> (interface () get-items get-item))
               (define list-box<%>
                 (interface
                   ()
                   number-of-visible-items
                   clear
                   delete
                   set
                   show
                   enable
                   get-x
                   get-y
                   append
                   get-selections
                   find-string
                   get-selection
                   set-selection
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-data
                   set-data
                   set-string
                   get-string
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   get-number
                   set-first-visible-item
                   get-string-selection
                   set-string-selection
                   is-selected?
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   select
                   get-top-level-window
                   get-first-visible-item
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define keymap<%>
                 (interface
                   ()
                   set-break-sequence-callback
                   get-double-click-interval
                   set-double-click-interval
                   remove-grab-key-function
                   set-grab-mouse-function
                   remove-grab-mouse-function
                   set-grab-key-function
                   remove-chained-keymap
                   handle-key-event
                   handle-mouse-event
                   add-key-function
                   add-mouse-function
                   set-error-callback
                   chain-to-keymap
                   break-sequence
                   map-function
                   implies-shift
                   call-function))
               (define key-event<%>
                 (interface
                   ()
                   set-y
                   set-x
                   get-x
                   get-y
                   set-time-stamp
                   get-time-stamp
                   set-control-down
                   get-control-down
                   set-alt-down
                   get-alt-down
                   set-meta-down
                   get-meta-down
                   set-shift-down
                   get-shift-down
                   set-key-code
                   get-key-code))
               (define image-snip<%>
                 (interface
                   ()
                   draw
                   split
                   match?
                   next
                   resize
                   copy
                   write
                   get-admin
                   set-admin
                   on-event
                   on-char
                   set-bitmap
                   load-file
                   get-style
                   set-style
                   size-cache-invalid
                   do-edit-operation
                   adjust-cursor
                   blink-caret
                   get-filename
                   set-offset
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   merge-with
                   previous
                   get-text
                   own-caret
                   get-extent
                   get-num-scroll-steps
                   get-scroll-step-offset
                   release-from-owner
                   find-scroll-step
                   get-filetype
                   set-snipclass
                   get-snipclass
                   partial-offset))
               (define horizontal-panel<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   set-alignment
                   get-alignment
                   focus
                   border
                   get-top-level-window
                   change-children
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
                   container-size
                   place-children
                   delete-child
                   client->screen
                   screen->client))
               (define horizontal-pane<%>
                 (interface
                   ()
                   get-parent
                   min-width
                   min-height
                   spacing
                   add-child
                   set-alignment
                   get-alignment
                   border
                   get-top-level-window
                   change-children
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   get-children
                   container-size
                   place-children
                   delete-child))
               (define gauge<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   set-range
                   get-range
                   get-value
                   set-value
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define frame<%>
                 (interface
                   ()
                   has-status-line?
                   resize
                   move
                   show
                   enable
                   center
                   get-x
                   get-y
                   get-menu-bar
                   on-activate
                   on-drop-file
                   set-status-text
                   create-status-line
                   get-client-size
                   on-subwindow-char
                   get-label
                   set-label
                   iconize
                   set-icon
                   maximize
                   on-close
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   can-close?
                   can-exit?
                   on-exit
                   on-focus
                   is-enabled?
                   set-alignment
                   get-alignment
                   focus
                   border
                   get-top-level-window
                   get-edit-target-window
                   get-edit-target-object
                   change-children
                   on-subwindow-event
                   get-focus-window
                   get-focus-object
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   get-children
                   container-size
                   place-children
                   delete-child
                   client->screen
                   screen->client
                   get-eventspace
                   is-iconized?))
               (define font-list<%> (interface () find-or-create-font))
               (define font<%>
                 (interface
                   ()
                   get-point-size
                   get-underlined
                   get-font-id
                   get-family
                   get-face
                   get-weight
                   get-style))
               (define event<%> (interface () set-time-stamp get-time-stamp))
               (define editor-wordbreak-map<%> (interface () set-map get-map))
               (define editor-stream-out-string-base<%>
                 (interface () write get-string tell seek bad?))
               (define editor-stream-out-base<%>
                 (interface () write tell seek bad?))
               (define editor-stream-out<%>
                 (interface () << ok? put-fixed jump-to put tell))
               (define editor-stream-in-string-base<%>
                 (interface () read tell seek skip bad?))
               (define editor-stream-in-base<%>
                 (interface () read tell seek skip bad?))
               (define editor-stream-in<%>
                 (interface
                   ()
                   >>
                   ok?
                   get-string
                   get-fixed
                   get-exact
                   jump-to
                   remove-boundary
                   get-inexact
                   set-boundary
                   get
                   tell
                   skip))
               (define editor-snip<%>
                 (interface
                   ()
                   draw
                   split
                   match?
                   next
                   resize
                   copy
                   write
                   get-admin
                   set-admin
                   set-editor
                   get-editor
                   on-event
                   on-char
                   get-style
                   set-style
                   size-cache-invalid
                   do-edit-operation
                   adjust-cursor
                   blink-caret
                   get-max-width
                   get-min-width
                   set-max-width
                   set-min-width
                   get-max-height
                   get-min-height
                   set-max-height
                   set-min-height
                   set-margin
                   get-margin
                   set-inset
                   get-inset
                   get-flags
                   get-count
                   set-count
                   set-flags
                   is-owned?
                   merge-with
                   previous
                   get-text
                   own-caret
                   get-extent
                   get-num-scroll-steps
                   get-scroll-step-offset
                   border-visible?
                   release-from-owner
                   find-scroll-step
                   show-border
                   set-snipclass
                   get-snipclass
                   partial-offset))
               (define editor-data-class-list<%>
                 (interface () find add nth number find-position))
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
                   get-dc
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   set-editor
                   get-editor
                   on-paint
                   on-event
                   on-char
                   popup-menu
                   on-scroll
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   force-display-focus
                   allow-scroll-to-last
                   call-as-primary-owner
                   warp-pointer
                   scroll-with-bottom-base
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   min-client-width
                   min-client-height
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client
                   lazy-refresh
                   set-line-count))
               (define editor-admin<%>
                 (interface
                   ()
                   get-dc
                   refresh-delayed?
                   needs-update
                   grab-caret
                   get-view
                   scroll-to
                   resized
                   get-max-view
                   update-cursor))
               (define dialog<%>
                 (interface
                   ()
                   resize
                   move
                   show
                   enable
                   center
                   get-x
                   get-y
                   on-activate
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-label
                   set-label
                   on-close
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   set-label-position
                   get-label-position
                   set-control-font
                   get-control-font
                   set-label-font
                   get-label-font
                   min-width
                   min-height
                   spacing
                   add-child
                   on-move
                   has-focus?
                   get-cursor
                   can-close?
                   can-exit?
                   on-exit
                   on-focus
                   is-enabled?
                   set-alignment
                   get-alignment
                   focus
                   border
                   get-top-level-window
                   get-edit-target-window
                   get-edit-target-object
                   change-children
                   on-subwindow-event
                   get-focus-window
                   get-focus-object
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   get-children
                   container-size
                   place-children
                   delete-child
                   client->screen
                   screen->client
                   get-eventspace))
               (define cursor<%> (interface () ok?))
               (define control-event<%>
                 (interface
                   ()
                   set-event-type
                   get-event-type
                   set-time-stamp
                   get-time-stamp))
               (define color<%>
                 (interface () ok? set red green blue copy-from))
               (define clipboard-client<%>
                 (interface () get-data add-type get-types being-replaced))
               (define choice<%>
                 (interface
                   ()
                   clear
                   show
                   enable
                   get-x
                   get-y
                   append
                   find-string
                   get-selection
                   set-selection
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-string
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   get-number
                   get-string-selection
                   set-string-selection
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define checkable-menu-item<%>
                 (interface
                   ()
                   delete
                   enable
                   check
                   get-label
                   set-label
                   get-parent
                   set-help-string
                   restore
                   is-enabled?
                   get-x-shortcut-prefix
                   set-x-shortcut-prefix
                   get-plain-label
                   get-help-string
                   go
                   is-deleted?
                   set-shortcut
                   get-shortcut
                   is-checked?))
               (define check-box<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   get-value
                   set-value
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define button<%>
                 (interface
                   ()
                   show
                   enable
                   get-x
                   get-y
                   on-drop-file
                   get-client-size
                   on-subwindow-char
                   command
                   get-label
                   set-label
                   set-cursor
                   is-shown?
                   get-size
                   refresh
                   get-parent
                   get-height
                   get-width
                   on-size
                   min-width
                   min-height
                   on-move
                   has-focus?
                   get-cursor
                   on-focus
                   is-enabled?
                   focus
                   get-top-level-window
                   on-subwindow-event
                   get-plain-label
                   accept-drop-files
                   stretchable-width
                   stretchable-height
                   horiz-margin
                   vert-margin
                   client->screen
                   screen->client))
               (define brush-list<%> (interface () find-or-create-brush))
               (define brush<%>
                 (interface
                   ()
                   get-stipple
                   set-stipple
                   get-color
                   set-color
                   get-style
                   set-style))
               (define bitmap-dc<%>
                 (interface
                   ()
                   clear
                   ok?
                   get-text-extent
                   set-bitmap
                   get-bitmap
                   get-pixel
                   set-pixel
                   draw-line
                   draw-point
                   draw-text
                   draw-arc
                   draw-lines
                   set-brush
                   set-font
                   set-pen
                   try-color
                   set-scale
                   set-origin
                   get-brush
                   get-font
                   get-pen
                   start-doc
                   start-page
                   end-doc
                   end-page
                   get-size
                   draw-rounded-rectangle
                   set-clipping-region
                   get-clipping-region
                   set-text-background
                   set-text-foreground
                   draw-bitmap-section
                   set-background-mode
                   get-background-mode
                   get-text-background
                   get-text-foreground
                   begin-set-pixel
                   set-clipping-rect
                   get-char-height
                   end-set-pixel
                   begin-drawing
                   end-drawing
                   draw-rectangle
                   draw-spline
                   draw-ellipse
                   draw-polygon
                   set-background
                   get-char-width
                   draw-bitmap
                   get-background))
               (define bitmap<%>
                 (interface
                   ()
                   ok?
                   get-depth
                   is-color?
                   load-file
                   save-file
                   get-height
                   get-width))
               (define -vertical-panel%
                 (class*
                   vertical-panel%
                   (panel<%> vertical-panel<%>)
                   args
                   (sequence (apply super-init args))))
               (define -vertical-pane%
                 (class*
                   vertical-pane%
                   (pane<%> vertical-pane<%>)
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
                   (snip<%> string-snip<%> tab-snip<%>)
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
                   (snip<%> string-snip<%>)
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
                   (event<%> scroll-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -region%
                 (class*
                   region%
                   (region<%>)
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
                   (event<%> mouse-event<%>)
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
                   (event<%> key-event<%>)
                   args
                   (sequence (apply super-init args))))
               (define -image-snip%
                 (class*
                   image-snip%
                   (image-snip<%> snip<%>)
                   args
                   (sequence (apply super-init args))))
               (define -horizontal-panel%
                 (class*
                   horizontal-panel%
                   (horizontal-panel<%> panel<%>)
                   args
                   (sequence (apply super-init args))))
               (define -horizontal-pane%
                 (class*
                   horizontal-pane%
                   (horizontal-pane<%> pane<%>)
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
                   (editor-stream-out-base<%> editor-stream-out-string-base<%>)
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
                   (editor-stream-in-base<%> editor-stream-in-string-base<%>)
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
                   (editor-snip<%> snip<%>)
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
                   (control-event<%> event<%>)
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
               (define -bitmap-dc%
                 (class*
                   bitmap-dc%
                   (bitmap-dc<%>)
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
                          pixel-dc<%>
                          mult-color<%>
                          mred@
                          message-box
                          menu-item<%>
                          menu-item-container<%>
                          menu-item%
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
