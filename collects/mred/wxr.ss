(unit/sig mred:wx^ (import)

; (define union (lambda (a b c) (or a b c)))

(define wx:set-afm-path (type: (str -> void)))
(define wx:const-event-type-scroll-bottom
  (type: num))
(define wx:const-snip-newline (type: num))
(define wx:const-default-dialog-style
  (type: num))
(define wx:new-id (type: (-> num)))
(define wx:register-id (type: (num -> void)))
(define wx:const-event-type-scroll-top
  (type: num))
(define wx:const-local-select (type: num))
(define wx:the-font-name-directory
  (type: wxfontnamedirectory-object))
(define wx:const-x-select (type: num))
(define wx:begin-busy-cursor
  (type: (optional wxcursor-object -> void)))
(define wx:const-mm-text (type: num))
(define wx:const-default-select (type: num))
(define wx:is-busy? (type: (-> bool)))
(define wx:const-cursor-spraycan (type: num))
(define wx:const-mm-lometric (type: num))
(define wx:const-break-for-line (type: num))
(define wx:end-busy-cursor (type: (-> void)))
(define wx:const-mm-metric (type: num))
(define wx:const-snip-after (type: num))
(define wx:display-size
  (type: ((box num) (box num) -> void)))
(define wx:const-cursor-sizenwse (type: num))
(define wx:const-mm-points (type: num))
(define wx:const-snip-before (type: num))
(define wx:find-window-by-label
  (type: (str optional
              (union null wxwindow-object)
              ->
              (union null wxwindow-object))))
(define wx:const-mm-twips (type: num))
(define wx:const-media-ff-copy (type: num))
(define wx:find-window-by-name
  (type: (str optional
              (union null wxwindow-object)
              ->
              (union null wxwindow-object))))
(define wx:const-cursor-sizenesw (type: num))
(define wx:const-colour (type: num))
(define wx:const-media-ff-same (type: num))
(define wx:strip-menu-codes (type: (str -> str)))
(define wx:const-xor (type: num))
(define wx:const-media-ff-text (type: num))
(define wx:get-free-memory (type: (-> num)))
(define wx:const-cursor-right-button (type: num))
(define wx:const-src-invert (type: num))
(define wx:const-media-ff-std (type: num))
(define wx:get-resource
  (type: (union str str)))
(define wx:const-set (type: num))
(define wx:const-media-ff-guess (type: num))
(define wx:write-resource
  (type: (union str str)))
(define wx:const-cursor-question-arrow
  (type: num))
(define wx:const-or-reverse (type: num))
(define wx:const-move-word (type: num))
(define wx:const-or-invert (type: num))
(define wx:const-move-page (type: num))
(define wx:yield
  (type: (optional semaphore -> bool)))
(define wx:const-cursor-point-right (type: num))
(define duplicate-key-event
  (type: (wxkeyevent-object -> wxkeyevent-object)))
(define wx:const-or (type: num))
(define wx:const-move-line (type: num))
(define wx:flush-display (type: (-> void)))
(define duplicate-mouse-event
  (type: (wxmouseevent-object -> wxmouseevent-object)))
(define wx:const-no-op (type: num))
(define wx:const-move-simple (type: num))
(define wx:const-cursor-point-left (type: num))
(define wx:const-nor (type: num))
(define wx:const-edit-kill (type: num))
(define wx:const-nand (type: num))
(define wx:const-edit-paste (type: num))
(define wx:const-cursor-painr-brush (type: num))
(define wx:const-invert (type: num))
(define wx:const-edit-copy (type: num))
(define wx:const-equiv (type: num))
(define wx:const-edit-cut (type: num))
(define wx:const-cursor-no-entry (type: num))
(define wx:const-copy (type: num))
(define wx:const-edit-clear (type: num))
(define wx:const-clear (type: num))
(define wx:const-edit-redo (type: num))
(define wx:const-cursor-middle-button
  (type: num))
(define wx:const-and-reverse (type: num))
(define wx:const-size-auto (type: num))
(define wx:const-edit-undo (type: num))
(define wx:const-and-invert (type: num))
(define wx:const-process-enter (type: num))
(define wx:const-cursor-magnifier (type: num))
(define wx:const-and (type: num))
(define wx:const-always-sb (type: num))
(define wx:const-cursor-watch (type: num))
(define wx:const-winding-rule (type: num))
(define wx:const-bitmap-type-pict-resource
  (type: num))
(define wx:const-needed-sb (type: num))
(define wx:const-cursor-wait (type: num))
(define wx:const-cursor-left-button (type: num))
(define wx:const-oddeven-rule (type: num))
(define wx:const-sb-mask (type: num))
(define wx:const-cursor-sizing (type: num))
(define wx:const-k-scroll (type: num))
(define wx:const-bitmap-type-pict (type: num))
(define wx:const-extended (type: num))
(define wx:get-media-print-margin
  (type: (optional
           (union null (box num))
           optional
           (union null (box num))
           ->
           void)))
(define wx:const-cursor-sizewe (type: num))
(define wx:const-cursor-bullseye (type: num))
(define wx:const-k-numlock (type: num))
(define wx:const-multiple (type: num))
(define wx:const-cursor-sizens (type: num))
(define wx:const-k-f24 (type: num))
(define wx:const-bitmap-type-xpm (type: num))
(define wx:const-single (type: num))
(define wx:set-media-print-margin
  (type: (optional num optional num -> void)))
(define wx:const-pos-use-minus-one (type: num))
(define wx:const-cursor-pencil (type: num))
(define wx:const-k-f23 (type: num))
(define wx:const-multiple-mask (type: num))
(define wx:const-cursor-ibeam (type: num))
(define wx:const-k-f22 (type: num))
(define wx:const-bitmap-type-xbm (type: num))
(define wx:read-media-global-header
  (type: (wxmediastreamin-object -> bool)))
(define wx:const-cursor-hand (type: num))
(define wx:const-horizontal-hatch (type: num))
(define wx:const-k-f21 (type: num))
(define wx:const-size-use-exsiting (type: num))
(define wx:const-cursor-cross (type: num))
(define wx:const-k-f20 (type: num))
(define wx:const-bitmap-type-gif (type: num))
(define wx:read-media-global-footer
  (type: (wxmediastreamin-object -> bool)))
(define wx:const-cursor-char (type: num))
(define wx:const-fdiagonal-hatch (type: num))
(define wx:const-k-f19 (type: num))
(define wx:const-cursor-arrow (type: num))
(define wx:const-k-f18 (type: num))
(define wx:const-bitmap-type-bmp-resource
  (type: num))
(define wx:write-media-global-header
  (type: (wxmediastreamout-object -> bool)))
(define wx:const-size-auto-height (type: num))
(define wx:const-crossdiag-hatch (type: num))
(define wx:const-k-f17 (type: num))
(define wx:const-k-f16 (type: num))
(define wx:const-bitmap-type-bmp (type: num))
(define wx:write-media-global-footer
  (type: (wxmediastreamout-object -> bool)))
(define wx:the-pen-list (type: wxpenlist-object))
(define wx:const-bdiagonal-hatch (type: num))
(define wx:const-k-f15 (type: num))
(define wx:const-editable (type: num))
(define wx:const-size-auto-width (type: num))
(define wx:const-k-f14 (type: num))
(define wx:const-bitmap-discard-colourmap
  (type: num))
(define wx:add-media-buffer-functions
  (type: (wxkeymap-object -> void)))
(define wx:const-k-f13 (type: num))
(define wx:const-dot-dash (type: num))
(define wx:const-k-f12 (type: num))
(define wx:const-bitmap-type-default (type: num))
(define wx:add-media-editor-functions
  (type: (wxkeymap-object -> void)))
(define wx:const-short-dash (type: num))
(define wx:const-k-f11 (type: num))
(define wx:const-long-dash (type: num))
(define wx:const-k-f10 (type: num))
(define wx:add-media-pasteboard-functions
  (type: (wxkeymap-object -> void)))
(define wx:const-dot (type: num))
(define wx:const-k-f9 (type: num))
(define wx:const-cap-butt (type: num))
(define wx:const-k-f8 (type: num))
(define wx:media-set-x-selection-mode
  (type: (bool -> void)))
(define wx:const-cap-projecting (type: num))
(define wx:const-k-f7 (type: num))
(define wx:const-cap-round (type: num))
(define wx:const-join-round (type: num))
(define wx:const-k-f6 (type: num))
(define wx:the-style-list
  (type: wxstylelist-object))
(define wx:get-the-snip-class-list
  (type: (-> wxsnipclasslist-object)))
(define wx:const-join-miter (type: num))
(define wx:const-k-f5 (type: num))
(define wx:const-snip-draw-show-inactive-caret
  (type: num))
(define wx:const-join-bevel (type: num))
(define wx:const-k-f4 (type: num))
(define wx:const-k-f3 (type: num))
(define wx:const-backingstore (type: num))
(define wx:const-snip-draw-show-caret
  (type: num))
(define wx:const-k-f2 (type: num))
(define wx:the-clipboard
  (type: (union null wxclipboard-object)))
(define wx:const-retained (type: num))
(define wx:get-the-buffer-data-class-list
  (type: (-> wxbufferdataclasslist-object)))
(define wx:const-k-f1 (type: num))
(define wx:const-allow-auto-resize (type: num))
(define wx:const-border (type: num))
(define wx:const-snip-draw-no-caret (type: num))
(define wx:exit (type: (-> void)))
(define wx:const-k-divide (type: num))
(define wx:bell (type: (-> void)))
(define wx:const-focus-immediate (type: num))
(define wx:const-change-normal-colour
  (type: num))
(define wx:the-brush-list
  (type: wxbrushlist-object))
(define wx:const-k-decimal (type: num))
(define wx:const-tiny-caption-vert (type: num))
(define wx:const-media-ff-text-force-cr
  (type: num))
(define wx:const-k-subtract (type: num))
(define wx:const-pasteboard-buffer (type: num))
(define wx:const-k-separator (type: num))
(define wx:const-tiny-caption-horiz (type: num))
(define wx:const-change-alignment (type: num))
(define wx:const-edit-select-all (type: num))
(define wx:const-stipple (type: num))
(define wx:const-k-add (type: num))
(define wx:const-vertical-hatch (type: num))
(define wx:const-k-multiply (type: num))
(define wx:const-edit-insert-image (type: num))
(define wx:const-cross-hatch (type: num))
(define wx:const-k-numpad9 (type: num))
(define wx:const-change-toggle-underline
  (type: num))
(define wx:const-solid (type: num))
(define wx:const-k-numpad8 (type: num))
(define wx:const-event-type-right-dclick
  (type: num))
(define wx:const-edit-insert-graphic-box
  (type: num))
(define wx:const-transparent (type: num))
(define wx:const-k-numpad7 (type: num))
(define wx:const-k-numpad6 (type: num))
(define wx:const-event-type-middle-dclick
  (type: num))
(define wx:const-change-toggle-weight
  (type: num))
(define wx:const-edit-insert-text-box
  (type: num))
(define wx:const-k-numpad5 (type: num))
(define wx:const-k-numpad4 (type: num))
(define wx:const-event-type-left-dclick
  (type: num))
(define wx:const-k-numpad3 (type: num))
(define wx:const-change-toggle-style (type: num))
(define wx:const-k-numpad2 (type: num))
(define wx:const-event-type-leave-window
  (type: num))
(define wx:const-k-numpad1 (type: num))
(define wx:const-align-center (type: num))
(define wx:const-k-numpad0 (type: num))
(define wx:const-event-type-enter-window
  (type: num))
(define wx:const-align-bottom (type: num))
(define wx:const-k-help (type: num))
(define wx:const-align-top (type: num))
(define wx:const-k-insert (type: num))
(define wx:const-event-type-motion (type: num))
(define wx:const-change-family (type: num))
(define wx:the-colour-database
  (type: wxcolourdatabase-object))
(define wx:const-k-snapshot (type: num))
(define wx:const-change-smaller (type: num))
(define wx:const-k-execute (type: num))
(define wx:const-event-type-right-up (type: num))
(define wx:const-change-bigger (type: num))
(define wx:const-k-print (type: num))
(define wx:const-change-italic (type: num))
(define wx:const-break-for-user-2 (type: num))
(define wx:const-k-select (type: num))
(define wx:const-event-type-right-down
  (type: num))
(define wx:const-change-bold (type: num))
(define wx:const-k-down (type: num))
(define wx:const-change-size (type: num))
(define wx:const-break-for-user-1 (type: num))
(define wx:hourglass-cursor
  (type: wxcursor-object))
(define wx:const-k-right (type: num))
(define wx:const-event-type-middle-up
  (type: num))
(define wx:const-change-weight (type: num))
(define wx:const-hide-readonly (type: num))
(define wx:the-media-wordbreak-map
  (type: wxmediawordbreakmap-object))
(define wx:const-k-up (type: num))
(define wx:const-change-style (type: num))
(define wx:const-break-for-selection (type: num))
(define wx:const-save (type: num))
(define wx:the-font-list
  (type: wxfontlist-object))
(define wx:const-k-left (type: num))
(define wx:const-event-type-middle-down
  (type: num))
(define wx:const-change-normal (type: num))
(define wx:const-open (type: num))
(define wx:const-k-home (type: num))
(define wx:const-change-nothing (type: num))
(define wx:const-break-for-caret (type: num))
(define wx:const-ps-printer (type: num))
(define wx:const-k-end (type: num))
(define wx:const-event-type-left-up (type: num))
(define wx:const-base (type: num))
(define wx:const-ps-file (type: num))
(define wx:const-slant (type: num))
(define wx:const-k-next (type: num))
(define wx:const-snip-after-or-null (type: num))
(define wx:const-ps-preview (type: num))
(define wx:const-italic (type: num))
(define wx:const-k-prior (type: num))
(define wx:const-event-type-left-down
  (type: num))
(define wx:const-ps-landscape (type: num))
(define wx:const-bold (type: num))
(define wx:const-k-capital (type: num))
(define wx:const-snip-before-or-null (type: num))
(define wx:const-ps-portrait (type: num))
(define wx:const-light (type: num))
(define wx:const-k-pause (type: num))
(define wx:const-event-type-char (type: num))
(define wx:const-focus-global (type: num))
(define wx:const-centre (type: num))
(define wx:const-normal (type: num))
(define wx:const-k-menu (type: num))
(define wx:const-focus-display (type: num))
(define wx:const-center (type: num))
(define wx:const-system (type: num))
(define wx:const-k-control (type: num))
(define wx:const-event-type-virt-listbox-command
  (type: num))
(define wx:const-edit-buffer (type: num))
(define wx:const-change-underline (type: num))
(define wx:const-icon-mask (type: num))
(define wx:const-teletype (type: num))
(define wx:const-k-shift (type: num))
(define wx:const-icon-asterisk (type: num))
(define wx:const-modern (type: num))
(define wx:const-k-clear (type: num))
(define wx:const-event-type-scrollbar-command
  (type: num))
(define wx:const-icon-stop (type: num))
(define wx:const-swiss (type: num))
(define wx:const-k-mbutton (type: num))
(define wx:const-icon-question (type: num))
(define wx:const-script (type: num))
(define wx:const-k-cancel (type: num))
(define wx:const-event-type-kill-focus
  (type: num))
(define wx:const-icon-hand (type: num))
(define wx:const-mcanvas-hide-v-scroll
  (type: num))
(define wx:const-roman (type: num))
(define wx:const-k-rbutton (type: num))
(define wx:const-readonly (type: num))
(define wx:const-no (type: num))
(define wx:const-decorative (type: num))
(define wx:const-k-lbutton (type: num))
(define wx:const-event-type-set-focus
  (type: num))
(define wx:const-yes (type: num))
(define wx:const-mcanvas-hide-h-scroll
  (type: num))
(define wx:const-default (type: num))
(define wx:const-k-start (type: num))
(define wx:const-cancel (type: num))
(define wx:const-k-delete (type: num))
(define wx:const-event-type-text-enter-command
  (type: num))
(define wx:const-caption (type: num))
(define wx:const-yes-no (type: num))
(define wx:const-mcanvas-no-v-scroll (type: num))
(define wx:const-k-space (type: num))
(define wx:const-ok (type: num))
(define wx:const-hscroll (type: num))
(define wx:const-k-escape (type: num))
(define wx:const-event-type-radiobox-command
  (type: num))
(define wx:const-msnipbox-yinset (type: num))
(define wx:concat-files
  (type: (str str str -> bool)))
(define wx:const-mcanvas-no-h-scroll (type: num))
(define wx:const-k-return (type: num))
(define wx:const-vscroll (type: num))
(define wx:copy-file (type: (str str -> bool)))
(define wx:const-k-tab (type: num))
(define wx:const-event-type-slider-command
  (type: num))
(define wx:const-msnipbox-xinset (type: num))
(define wx:get-temp-file-name
  (type: (str -> str)))
(define wx:const-password (type: num))
(define wx:const-k-back (type: num))
(define wx:get-host-name (type: (-> str)))
(define wx:const-event-type-menu-command
  (type: num))
(define wx:const-msnipbox-ymargin (type: num))
(define wx:get-email-address (type: (-> str)))
(define wx:get-user-id (type: (-> str)))
(define wx:const-event-type-multitext-command
  (type: num))
(define wx:const-msnipbox-xmargin (type: num))
(define wx:get-user-name (type: (-> str)))
(define wx:string-match?
  (type: (str str optional bool optional bool -> bool)))
(define wx:const-event-type-text-command
  (type: num))
(define wx:const-snip-uses-buffer-path
  (type: num))
(define wx:set-display (type: (str -> bool)))
(define wx:const-overwrite-prompt (type: num))
(define wx:get-display-name (type: (-> str)))
(define wx:const-vertical (type: num))
(define wx:const-event-type-listbox-command
  (type: num))
(define wx:const-snip-height-depends-on-y
  (type: num))
(define wx:file-selector
  (type: (str optional
              str
              optional
              str
              optional
              str
              optional
              str
              optional
              num
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              ->
              str)))
(define wx:const-icon-information (type: num))
(define wx:const-horizontal (type: num))
(define wx:message-box
  (type: (str optional
              str
              optional
              num
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              ->
              num)))
(define wx:const-both (type: num))
(define wx:const-event-type-choice-command
  (type: num))
(define wx:const-snip-width-depends-on-y
  (type: num))
(define wx:get-text-from-user
  (type: (str optional
              str
              optional
              str
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              optional
              bool
              ->
              str)))
(define wx:const-icon-exclamation (type: num))
(define wx:const-default-frame (type: num))
(define wx:get-multiple-choice
  (type: (str str
              (listof str)
              num
              (listof num)
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              optional
              bool
              optional
              num
              optional
              num
              ->
              num)))
(define wx:const-resize-border (type: num))
(define wx:const-event-type-checkbox-command
  (type: num))
(define wx:const-snip-height-depends-on-x
  (type: num))
(define wx:get-single-choice
  (type: (str str
              (listof str)
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              optional
              bool
              optional
              num
              optional
              num
              ->
              str)))
(define wx:get-single-choice-index
  (type: (str str
              (listof str)
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              optional
              bool
              optional
              num
              optional
              num
              ->
              num)))
(define wx:const-resize-box (type: num))
(define wx:colour-display? (type: (-> bool)))
(define wx:const-maximize-box (type: num))
(define wx:const-event-type-button-command
  (type: num))
(define wx:const-snip-width-depends-on-x
  (type: num))
(define wx:display-depth (type: (-> num)))
(define wx:get-single-choice-data
  (type: (str str
              (listof str)
              (listof str)
              optional
              (union null wxwindow-object)
              optional
              num
              optional
              num
              optional
              bool
              optional
              num
              optional
              num
              ->
              str)))
(define wx:const-minimize-box (type: num))
(define wx:set-cursor
  (type: (wxcursor-object -> void)))
(define wx:const-system-menu (type: num))
(define wx:const-type-mouse-event (type: num))
(define wx:const-snip-handles-events (type: num))
(define wx:get-printer-command (type: (-> str)))
(define wx:make-meta-file-placeable
  (type: (str num num num num num -> bool)))
(define wx:const-thick-frame (type: num))
(define wx:const-type-key-event (type: num))
(define wx:get-printer-file (type: (-> str)))
(define wx:const-mdi-child (type: num))
(define wx:const-type-command-event (type: num))
(define wx:const-snip-hard-newline (type: num))
(define wx:get-printer-mode (type: (-> num)))
(define wx:get-printer-preview-command
  (type: (-> str)))
(define wx:const-mdi-parent (type: num))
(define wx:get-printer-options (type: (-> str)))
(define wx:const-sdi (type: num))
(define wx:const-event-type-scroll-thumbtrack
  (type: num))
(define wx:const-snip-can-append (type: num))
(define wx:get-printer-scaling
  (type: ((box num) (box num) -> void)))
(define wx:get-printer-orientation
  (type: (-> num)))
(define wx:const-maximize (type: num))
(define wx:get-print-paper-name (type: (-> str)))
(define wx:const-minimize (type: num))
(define wx:const-event-type-scroll-pagedown
  (type: num))
(define wx:get-afm-path (type: (-> str)))
(define wx:get-printer-translation
  (type: ((box num) (box num) -> void)))
(define wx:const-iconize (type: num))
(define wx:set-printer-command
  (type: (str -> void)))
(define wx:const-stay-on-top (type: num))
(define wx:const-event-type-scroll-pageup
  (type: num))
(define wx:set-printer-file
  (type: (str -> void)))
(define wx:set-printer-preview-command
  (type: (str -> void)))
(define wx:set-printer-mode
  (type: (num -> void)))
(define wx:const-event-type-scroll-linedown
  (type: num))
(define wx:set-printer-options
  (type: (str -> void)))
(define wx:set-printer-orientation
  (type: (num -> void)))
(define wx:const-snip-anchored (type: num))
(define wx:set-printer-scaling
  (type: (num num -> void)))
(define wx:const-event-type-scroll-lineup
  (type: num))
(define wx:const-snip-invisible (type: num))
(define wx:set-print-paper-name
  (type: (str -> void)))
(define wx:set-printer-translation
  (type: (num num -> void)))
(define wx:const-snip-is-text (type: num))

(define wx:object% (class null () (public)))

(define wx:window%
  (class wx:object%
         ()
         (public
           (get-text-extent
             (type: (str (box num)
                         (box num)
                         optional
                         (union null (box num))
                         optional
                         (union null (box num))
                         optional
                         (union null wxfont-object)
                         optional
                         bool
                         ->
                         void)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (is-shown? (type: (-> bool)))
           (get-height (type: (-> num)))
           (get-char-height (type: (-> num)))
           (get-size (type: ((box num) (box num) -> void)))
           (get-width (type: (-> num)))
           (get-char-width (type: (-> num)))
           (set-focus (type: (-> void)))
           (set-size
             (type: (num num num num optional num -> void)))
           (on-size (type: (num num -> void)))
           (show (type: (bool -> void)))
           (enable (type: (bool -> void)))
           (get-label (type: (-> str)))
           (center (type: (num -> void)))
           (get-grand-parent
             (type: (-> (union null wxwindow-object))))
           (get-client-size
             (type: ((box num) (box num) -> void)))
           (centre (type: (optional num -> void)))
           (client-to-screen
             (type: ((box num) (box num) -> void)))
           (refresh (type: (-> void)))
           (screen-to-client
             (type: ((box num) (box num) -> void)))
           (get-parent
             (type: (-> (union null wxwindow-object))))
           (on-kill-focus (type: (-> void)))
           (set-cursor
             (type: (wxcursor-object -> (union null wxcursor-object))))
           (on-set-focus (type: (-> void)))
           (fit (type: (-> void)))
           (get-y (type: (-> num)))
           (get-x (type: (-> num)))
           (move (type: (num num -> void)))
           (get-position
             (type: ((box num) (box num) -> void)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool))))))

(define wx:item%
  (class wx:window%
         ()
         (public
           (get-button-colour (type: (-> wxcolour-object)))
           (set-background-colour
             (type: (union (wxcolour-object -> void)
                           (wxcolour-object -> void))))
           (command (type: (wxcommandevent-object -> void)))
           (get-label-colour (type: (-> wxcolour-object)))
           (set-label-colour
             (type: (union (wxcolour-object -> void)
                           (wxcolour-object -> void))))
           (set-label (type: (str -> void)))
           (get-background-colour
             (type: (-> wxcolour-object)))
           (set-button-colour
             (type: (union (wxcolour-object -> void)
                           (wxcolour-object -> void))))
           (get-char-height (type: (-> num)))
           (get-label (type: (-> str)))
           (get-char-width (type: (-> num))))))

(define wx:message%
  (class wx:item%
         (init1-arg-wxpanel-object-or-init1-arg-wxpanel-object
           init2-arg-str-or-init2-arg-wxbitmap-object
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: str)))
         (public
           (set-label
             (type: (union (wxbitmap-object -> void) (str -> void)))))))

(define wx:canvas%
  (class wx:window%
         (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: str)))
         (public
           (enable-scrolling (type: (bool bool -> void)))
           (set-clipping-region
             (type: (num num num num -> void)))
           (on-paint (type: (-> void)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (get-clipping-region
             (type: ((box num) (box num) (box num) (box num) -> void)))
           (on-event (type: (wxmouseevent-object -> void)))
           (begin-drawing (type: (-> void)))
           (set-background (type: (wxbrush-object -> void)))
           (end-drawing (type: (-> void)))
           (on-char
             (type: (union (wxkeyevent-object -> void)
                           (wxkeyevent-object -> void))))
           (popup-menu
             (type: (wxmenu-object num num -> bool)))
           (draw-line (type: (num num num num -> void)))
           (set-brush (type: (wxbrush-object -> void)))
           (scroll (type: (num num -> void)))
           (draw-point (type: (num num -> void)))
           (set-pen (type: (wxpen-object -> void)))
           (get-virtual-size
             (type: ((box num) (box num) -> void)))
           (draw-rectangle
             (type: (num num num num -> void)))
           (set-font (type: (wxfont-object -> void)))
           (is-retained? (type: (-> bool)))
           (draw-rounded-rectangle
             (type: (num num num num optional num -> void)))
           (set-logical-function (type: (num -> void)))
           (on-size (type: (num num -> void)))
           (set-scrollbars
             (type: (num num
                         num
                         num
                         num
                         num
                         optional
                         num
                         optional
                         num
                         optional
                         bool
                         ->
                         void)))
           (draw-spline
             (type: (num num num num num num -> void)))
           (get-dc (type: (-> wxcanvasdc-object)))
           (view-start
             (type: ((box num) (box num) -> void)))
           (draw-text
             (type: (str num num optional bool -> void)))
           (set-text-background
             (type: (wxcolour-object -> void)))
           (warp-pointer (type: (num num -> void)))
           (draw-arc
             (type: (num num num num num num -> void)))
           (on-kill-focus (type: (-> void)))
           (draw-ellipse (type: (num num num num -> void)))
           (set-text-foreground
             (type: (wxcolour-object -> void)))
           (on-set-focus (type: (-> void)))
           (draw-lines
             (type: (wxpoint-object
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (clear (type: (-> void)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (draw-polygon
             (type: (wxpoint-object
                      optional
                      num
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (get-scroll-units-per-page
             (type: ((box num) (box num) -> void)))
           (allow-double-click (type: (bool -> void)))
           (destroy-clipping-region (type: (-> void))))))

(define wx:clipboard-client%
  (class wx:object%
         ()
         (public
           (get-data (type: (str -> str)))
           (add-type (type: (str -> void)))
           (get-types (type: (-> str)))
           (being-replaced (type: (-> void))))))

(define wx:dc%
  (class wx:object%
         ()
         (public
           (get-text-background
             (type: (-> wxcolour-object)))
           (get-pen (type: (-> wxpen-object)))
           (set-clipping-region
             (type: (num num num num -> void)))
           (max-x (type: (-> num)))
           (get-text-extent
             (type: (str (box num)
                         (box num)
                         optional
                         (union null (box num))
                         optional
                         (union null (box num))
                         optional
                         (union null wxfont-object)
                         optional
                         bool
                         ->
                         void)))
           (get-text-foreground
             (type: (-> wxcolour-object)))
           (max-y (type: (-> num)))
           (get-clipping-region
             (type: ((box num) (box num) (box num) (box num) -> void)))
           (min-x (type: (-> num)))
           (get-char-height (type: (-> num)))
           (get-size (type: ((box num) (box num) -> void)))
           (min-y (type: (-> num)))
           (set-background (type: (wxbrush-object -> void)))
           (end-doc (type: (-> void)))
           (get-char-width (type: (-> num)))
           (end-drawing (type: (-> void)))
           (begin-drawing (type: (-> void)))
           (set-optimization (type: (bool -> void)))
           (draw-line (type: (num num num num -> void)))
           (set-brush (type: (wxbrush-object -> void)))
           (device-to-logical-x (type: (num -> num)))
           (draw-point (type: (num num -> void)))
           (set-pen (type: (wxpen-object -> void)))
           (draw-icon
             (type: (wxicon-object num num -> void)))
           (device-to-logical-y (type: (num -> num)))
           (draw-rectangle
             (type: (num num num num -> void)))
           (set-font (type: (wxfont-object -> void)))
           (draw-ellipse (type: (num num num num -> void)))
           (try-colour
             (type: (wxcolour-object wxcolour-object -> void)))
           (logical-to-device-x (type: (num -> num)))
           (draw-rounded-rectangle
             (type: (num num num num optional num -> void)))
           (set-logical-function (type: (num -> void)))
           (set-colour-map
             (type: (wxcolourmap-object -> void)))
           (set-map-mode (type: (num -> void)))
           (logical-to-device-y (type: (num -> num)))
           (draw-spline
             (type: (num num num num num num -> void)))
           (set-background-mode (type: (num -> void)))
           (start-doc (type: (str -> bool)))
           (draw-text
             (type: (str num num optional bool -> void)))
           (set-text-background
             (type: (wxcolour-object -> void)))
           (set-user-scale (type: (num num -> void)))
           (start-page (type: (-> void)))
           (draw-arc
             (type: (num num num num num num -> void)))
           (get-background (type: (-> wxbrush-object)))
           (end-page (type: (-> void)))
           (ok? (type: (-> bool)))
           (set-text-foreground
             (type: (wxcolour-object -> void)))
           (get-brush (type: (-> wxbrush-object)))
           (draw-lines
             (type: (wxpoint-object
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (clear (type: (-> void)))
           (get-font (type: (-> wxfont-object)))
           (get-logical-function (type: (-> num)))
           (draw-polygon
             (type: (wxpoint-object
                      optional
                      num
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (get-map-mode (type: (-> num)))
           (destroy-clipping-region (type: (-> void)))
           (blit (type: (num num
                             num
                             num
                             wxcanvasdc-object
                             num
                             num
                             optional
                             num
                             ->
                             bool))))))

(define wx:media-stream-in%
  (class wx:object%
         (init1-arg-wxmediastreaminbase-object)
         (public
           (jump-to (type: (num -> void)))
           (tell (type: (-> num)))
           (remove-boundary (type: (-> void)))
           (get-fixed
             (type: ((box num) -> wxmediastreamin-object)))
           (get (type: (union ((box num) -> wxmediastreamin-object)
                              ((box num) -> wxmediastreamin-object))))
           (skip (type: (num -> void)))
           (get-exact (type: (-> num)))
           (get-inexact (type: (-> num)))
           (ok? (type: (-> bool)))
           (get-string
             (type: (optional (union null (box num)) -> str)))
           (>> (type: (union ((box num) -> wxmediastreamin-object)
                             ((box num) -> wxmediastreamin-object))))
           (set-boundary (type: (num -> void))))))

(define wx:cursor%
  (class wx:object%
         (init1-arg-str-or-init1-arg-num
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num)))
         (public (ok? (type: (-> bool))))))

(define wx:media-stream-in-base%
  (class wx:object%
         ()
         (public
           (read (type: ((listof char) -> num)))
           (tell (type: (-> num)))
           (seek (type: (num -> void)))
           (skip (type: (num -> void)))
           (bad? (type: (-> bool))))))

(define wx:list-box%
  (class wx:item%
         (init1-arg-wxpanel-object
           |init2-arg-(wxlistbox-object wxevent-object -> void)|
           init3-arg-str
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: (listof str)))
           (init10 (type: num))
           (init11 (type: str)))
         (public
           (find-string (type: (str -> num)))
           (get-selection (type: (-> num)))
           (get-string-selection (type: (-> str)))
           (set-selection
             (type: (num optional bool -> void)))
           (set-string-selection (type: (str -> void)))
           (delete (type: (num -> void)))
           (clear (type: (-> void)))
           (get-string (type: (num -> str)))
           (number (type: (-> num)))
           (deselect (type: (num -> void)))
           (selected? (type: (num -> bool)))
           (set (type: ((listof str) -> void)))
           (get-client-data (type: (num -> str)))
           (set-client-data (type: (num str -> void)))
           (get-selections (type: ((box (box num)) -> num)))
           (set-first-item
             (type: (union (num -> void) (str -> void))))
           (set-string (type: (num str -> void)))
           (append
             (type: (union (str -> void) (str str -> void)))))))

(define wx:int-point%
  (class wx:object%
         (init1-arg-num init2-arg-num)
         (public
           (get-y (type: (union (num -> void) (-> num))))
           (get-x (type: (union (num -> void) (-> num)))))))

(define wx:brush%
  (class wx:object%
         (init1-arg-wxcolour-object-or-init1-arg-str
           init2-arg-num-or-init2-arg-num)
         (public
           (set-colour
             (type: (union (wxcolour-object -> void)
                           (union (str -> void) (num num num -> void)))))
           (get-stipple (type: (-> wxbitmap-object)))
           (get-style (type: (-> num)))
           (get-colour (type: (-> wxcolour-object)))
           (set-style (type: (num -> void)))
           (set-stipple (type: (wxbitmap-object -> void))))))

(define wx:mult-colour%
  (class wx:object%
         ()
         (public
           (get-b (type: (union (num -> void) (-> num))))
           (get-g (type: (union (num -> void) (-> num))))
           (get (type: ((box num) (box num) (box num) -> void)))
           (set (type: (num num num -> void)))
           (get-r (type: (union (num -> void) (-> num)))))))

(define wx:media-wordbreak-map%
  (class wx:object%
         ()
         (public
           (adjust-usage (type: (bool -> void)))
           (is-used? (type: (-> bool)))
           (get-map (type: (num -> num)))
           (set-map (type: (num num -> void))))))

(define wx:panel%
  (class wx:canvas%
         (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: str)))
         (public
           (get-item-cursor
             (type: ((box num) (box num) -> void)))
           (get-child (type: (num -> wxobject-object)))
           (on-paint (type: (union (-> void) (-> void))))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (set-item-cursor (type: (num num -> void)))
           (on-event
             (type: (union (wxmouseevent-object -> void)
                           (wxmouseevent-object -> void))))
           (fit (type: (-> void)))
           (get-button-colour (type: (-> wxcolour-object)))
           (on-default-action
             (type: (wxitem-object -> void)))
           (on-char
             (type: (union (wxkeyevent-object -> void)
                           (wxkeyevent-object -> void))))
           (get-label-colour (type: (-> wxcolour-object)))
           (set-label-position (type: (num -> void)))
           (get-background-colour
             (type: (-> wxcolour-object)))
           (get-horizontal-spacing (type: (-> num)))
           (get-vertical-spacing (type: (-> num)))
           (set-background-colour
             (type: (wxcolour-object -> void)))
           (set-horizontal-spacing (type: (num -> void)))
           (on-size (type: (num num -> void)))
           (set-label-colour
             (type: (wxcolour-object -> void)))
           (set-vertical-spacing (type: (num -> void)))
           (set-button-colour
             (type: (wxcolour-object -> void)))
           (new-line
             (type: (union (-> void) (num -> void))))
           (get-panel-dc (type: (-> wxdc-object)))
           (on-kill-focus (type: (-> void)))
           (set-button-font (type: (wxfont-object -> void)))
           (on-set-focus (type: (-> void)))
           (get-button-font (type: (-> wxfont-object)))
           (tab (type: (union (-> void) (num -> void))))
           (set-label-font (type: (wxfont-object -> void)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (get-label-font (type: (-> wxfont-object)))
           (get-default-item (type: (-> wxbutton-object)))
           (advance-cursor
             (type: (wxwindow-object -> void))))))

(define wx:point%
  (class wx:object%
         (init1-arg-num init2-arg-num)
         (public
           (get-y (type: (union (num -> void) (-> num))))
           (get-x (type: (union (num -> void) (-> num)))))))

(define wx:snip%
  (class wx:object%
         ()
         (public
           (get-flags (type: (-> num)))
           (get-count (type: (-> num)))
           (own-caret (type: (bool -> void)))
           (set-count (type: (num -> void)))
           (on-event
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxmouseevent-object
                      ->
                      void)))
           (get-extent
             (type: (wxdc-object
                      num
                      num
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      void)))
           (get-style (type: (-> wxstyle-object)))
           (size-cache-invalid (type: (-> void)))
           (set-flags (type: (num -> void)))
           (set-admin (type: (wxsnipadmin-object -> void)))
           (get-text (type: (num num optional bool -> str)))
           (on-char
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxkeyevent-object
                      ->
                      void)))
           (is-owned? (type: (-> bool)))
           (set-style (type: (wxstyle-object -> void)))
           (release-from-owner (type: (-> bool)))
           (get-admin (type: (-> wxsnipadmin-object)))
           (partial-offset
             (type: (wxdc-object num num num -> num)))
           (merge-with
             (type: (wxsnip-object -> wxsnip-object)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (do-font (type: (num optional bool -> void)))
           (previous
             (type: (-> (union null wxsnip-object))))
           (write (type: (wxmediastreamout-object -> void)))
           (draw (type: (wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (split (type: (num (box wxsnip-object)
                              (box wxsnip-object)
                              ->
                              void)))
           (copy (type: (-> wxsnip-object)))
           (match? (type: (wxsnip-object -> bool)))
           (next (type: (-> (union null wxsnip-object))))
           (resize (type: (num num -> bool)))
           (get-snipclass
             (type: (union ((union null wxsnipclass-object) -> void)
                           (-> (union null wxsnipclass-object))))))))

(define wx:bitmap%
  (class wx:object%
         (|init1-arg-(listof char)-or-init1-arg-num-or-init1-arg-str|
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num)))
         (public
           (load-file (type: (str optional num -> void)))
           (set-colour-map
             (type: ((union null wxcolourmap-object) -> void)))
           (get-depth (type: (-> num)))
           (get-height (type: (-> num)))
           (save-file
             (type: (str num
                         optional
                         (union null wxcolourmap-object)
                         ->
                         void)))
           (ok? (type: (-> bool)))
           (get-width (type: (-> num))))))

(define wx:font%
  (class wx:object%
         (init1-arg-num-or-init1-arg-num
           init2-arg-num-or-init2-arg-str
           init3-arg-num-or-init3-arg-num
           init4-arg-num-or-init4-arg-num
           (init5 (type: bool))
           (init6 (type: bool)))
         (public
           (get-family (type: (-> num)))
           (get-weight (type: (-> num)))
           (get-font-id (type: (-> num)))
           (get-underlined (type: (-> bool)))
           (get-style (type: (-> num)))
           (get-point-size (type: (-> num))))))

(define wx:style-list%
  (class wx:object%
         ()
         (public
           (copy (type: (wxstylelist-object -> void)))
           (find-or-create-join-style
             (type: ((union null wxstyle-object)
                     wxstyle-object
                     ->
                     wxstyle-object)))
           (find-named-style
             (type: (str -> wxstyle-object)))
           (adjust-usage (type: (bool -> void)))
           (new-named-style
             (type: (str (union null wxstyle-object)
                         ->
                         wxstyle-object)))
           (is-used? (type: (-> bool)))
           (clear (type: (-> void)))
           (number (type: (-> num)))
           (basic-style (type: (-> wxstyle-object)))
           (replace-named-style
             (type: (str (union null wxstyle-object)
                         ->
                         wxstyle-object)))
           (style-to-index (type: (wxstyle-object -> num)))
           (convert
             (type: (wxstyle-object -> wxstyle-object)))
           (find-or-create-style
             (type: ((union null wxstyle-object)
                     wxstyledelta-object
                     ->
                     wxstyle-object)))
           (index-to-style
             (type: (num -> (union null wxstyle-object)))))))

(define wx:media-admin%
  (class wx:object%
         ()
         (public
           (get-dc
             (type: (optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      (union null wxdc-object))))
           (get-view
             (type: ((union null (box num))
                     (union null (box num))
                     (union null (box num))
                     (union null (box num))
                     optional
                     bool
                     ->
                     void)))
           (set-cursor (type: (wxcursor-object -> void)))
           (resized (type: (bool -> void)))
           (scroll-to
             (type: (num num
                         num
                         num
                         optional
                         bool
                         optional
                         num
                         ->
                         bool)))
           (grab-caret (type: (optional num -> void)))
           (needs-update (type: (num num num num -> void))))))

(define wx:pen-list%
  (class wx:object%
         ()
         (public
           (find-or-create-pen
             (type: (union (wxcolour-object num num -> wxpen-object)
                           (str num num -> wxpen-object)))))))

(define wx:media-stream-in-string-base%
  (class wx:media-stream-in-base%
         (init1-arg-str)
         (public)))

(define wx:check-box%
  (class wx:item%
         (init1-arg-wxpanel-object-or-init1-arg-wxpanel-object
           |init2-arg-(wxcheckbox-object wxevent-object -> void)-or-init2-arg-(wxcheckbox-object wxevent-object -> void)|
           init3-arg-str-or-init3-arg-wxbitmap-object
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: str)))
         (public
           (set-value (type: (bool -> void)))
           (set-label
             (type: (union (wxbitmap-object -> void) (str -> void))))
           (get-value (type: (-> bool))))))

(define wx:media-stream-out-base%
  (class wx:object%
         ()
         (public
           (tell (type: (-> num)))
           (seek (type: (num -> void)))
           (bad? (type: (-> bool)))
           (write (type: ((listof char) -> void))))))

(define wx:gauge%
  (class wx:item%
         (init1-arg-wxpanel-object
           init2-arg-str
           init3-arg-num
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: str)))
         (public
           (set-value (type: (num -> void)))
           (set-range (type: (num -> void)))
           (get-range (type: (-> num)))
           (get-value (type: (-> num))))))

(define wx:image-snip%
  (class wx:snip%
         ((init1 (type: str))
          (init2 (type: num))
          (init3 (type: bool))
          (init4 (type: bool)))
         (public
           (own-caret (type: (bool -> void)))
           (on-event
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxmouseevent-object
                      ->
                      void)))
           (get-extent
             (type: (wxdc-object
                      num
                      num
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      void)))
           (size-cache-invalid (type: (-> void)))
           (set-admin (type: (wxsnipadmin-object -> void)))
           (get-text (type: (num num optional bool -> str)))
           (load-file
             (type: (str num optional bool optional bool -> void)))
           (on-char
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxkeyevent-object
                      ->
                      void)))
           (get-filetype (type: (-> num)))
           (set-bitmap (type: (wxbitmap-object -> void)))
           (partial-offset
             (type: (wxdc-object num num num -> num)))
           (set-offset (type: (num num -> void)))
           (merge-with
             (type: (wxsnip-object -> wxsnip-object)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (get-filename
             (type: ((union null (box bool)) -> str)))
           (do-font (type: (num optional bool -> void)))
           (write (type: (wxmediastreamout-object -> void)))
           (draw (type: (wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (split (type: (num (box wxsnip-object)
                              (box wxsnip-object)
                              ->
                              void)))
           (copy (type: (-> wxsnip-object)))
           (match? (type: (wxsnip-object -> bool)))
           (resize (type: (num num -> bool))))))

(define wx:menu-bar%
  (class wx:object%
         (|init1-arg-(listof wxmenu-object)|
           |init2-arg-(listof str)|)
         (public
           (get-help-string (type: (num -> str)))
           (checked? (type: (num -> bool)))
           (get-label-top (type: (num -> str)))
           (set-help-string (type: (num str -> void)))
           (set-label (type: (num str -> void)))
           (set-label-top (type: (num str -> void)))
           (get-title (type: (-> str)))
           (delete
             (type: (wxmenu-object optional num -> void)))
           (check (type: (num bool -> void)))
           (enable-top (type: (num bool -> void)))
           (enable (type: (num bool -> void)))
           (get-label (type: (num -> str)))
           (append (type: (wxmenu-object str -> void)))
           (find-menu-item (type: (str str -> num))))))

(define wx:colour-database%
  (class wx:object%
         ()
         (public
           (find-colour
             (type: (str -> (union null wxcolour-object))))
           (find-name (type: (wxcolour-object -> str)))
           (append (type: (str wxcolour-object -> void))))))

(define wx:text-snip%
  (class wx:snip%
         ((init1 (type: num)))
         (public
           (own-caret (type: (bool -> void)))
           (on-event
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxmouseevent-object
                      ->
                      void)))
           (get-extent
             (type: (wxdc-object
                      num
                      num
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      void)))
           (size-cache-invalid (type: (-> void)))
           (set-admin (type: (wxsnipadmin-object -> void)))
           (get-text (type: (num num optional bool -> str)))
           (on-char
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxkeyevent-object
                      ->
                      void)))
           (partial-offset
             (type: (wxdc-object num num num -> num)))
           (merge-with
             (type: (wxsnip-object -> wxsnip-object)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (do-font (type: (num optional bool -> void)))
           (write (type: (wxmediastreamout-object -> void)))
           (insert (type: (str num optional num -> void)))
           (read (type: (num wxmediastreamin-object -> void)))
           (split (type: (num (box wxsnip-object)
                              (box wxsnip-object)
                              ->
                              void)))
           (copy (type: (-> wxsnip-object)))
           (match? (type: (wxsnip-object -> bool)))
           (resize (type: (num num -> bool)))
           (draw (type: (wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void))))))

(define wx:colour-map%
  (class wx:object% () (public)))

(define wx:tab-snip%
  (class wx:text-snip%
         ()
         (public
           (own-caret (type: (bool -> void)))
           (on-event
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxmouseevent-object
                      ->
                      void)))
           (get-extent
             (type: (wxdc-object
                      num
                      num
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      void)))
           (size-cache-invalid (type: (-> void)))
           (set-admin (type: (wxsnipadmin-object -> void)))
           (get-text (type: (num num optional bool -> str)))
           (on-char
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxkeyevent-object
                      ->
                      void)))
           (partial-offset
             (type: (wxdc-object num num num -> num)))
           (merge-with
             (type: (wxsnip-object -> wxsnip-object)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (do-font (type: (num optional bool -> void)))
           (write (type: (wxmediastreamout-object -> void)))
           (draw (type: (wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (split (type: (num (box wxsnip-object)
                              (box wxsnip-object)
                              ->
                              void)))
           (copy (type: (-> wxsnip-object)))
           (match? (type: (wxsnip-object -> bool)))
           (resize (type: (num num -> bool))))))

(define wx:colour%
  (class wx:object%
         (init1-arg-num-or-init1-arg-str
           init2-arg-num
           init3-arg-num)
         (public
           (get (type: ((box num) (box num) (box num) -> void)))
           (set (type: (num num num -> void)))
           (red (type: (-> num)))
           (= (type: (wxcolour-object -> wxcolour-object)))
           (green (type: (-> num)))
           (ok? (type: (-> bool)))
           (blue (type: (-> num))))))

(define wx:event%
  (class wx:object%
         ()
         (public
           (get-event-object
             (type: (union (wxobject-object -> void)
                           (-> wxobject-object))))
           (get-event-type
             (type: (union (num -> void) (-> num))))
           (get-event-class
             (type: (union (num -> void) (-> num)))))))

(define wx:pen%
  (class wx:object%
         (init1-arg-wxcolour-object-or-init1-arg-str
           init2-arg-num-or-init2-arg-num
           init3-arg-num-or-init3-arg-num)
         (public
           (set-cap (type: (num -> void)))
           (set-colour
             (type: (union (wxcolour-object -> void)
                           (union (str -> void) (num num num -> void)))))
           (set-join (type: (num -> void)))
           (get-stipple (type: (-> wxbitmap-object)))
           (get-style (type: (-> num)))
           (set-width (type: (num -> void)))
           (get-colour (type: (-> wxcolour-object)))
           (set-style (type: (num -> void)))
           (set-stipple (type: (wxbitmap-object -> void)))
           (get-cap (type: (-> num)))
           (get-width (type: (-> num)))
           (get-join (type: (-> num))))))

(define wx:frame%
  (class wx:window%
         (|init1-arg-(union null wxframe-object)|
           init2-arg-str
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: str)))
         (public
           (set-menu-bar (type: (wxmenubar-object -> void)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (get-menu-bar
             (type: (union (-> (union null wxmenubar-object))
                           (-> (union null wxmenubar-object)))))
           (set-tool-bar
             (type: ((union null wxtoolbar-object) -> void)))
           (on-size (type: (num num -> void)))
           (get-tool-bar
             (type: (-> (union null wxtoolbar-object))))
           (set-status-text (type: (str -> void)))
           (iconized? (type: (-> bool)))
           (command (type: (num -> void)))
           (status-line-exists? (type: (-> bool)))
           (maximize (type: (bool -> void)))
           (iconize (type: (bool -> void)))
           (load-accelerators (type: (str -> void)))
           (on-kill-focus (type: (-> void)))
           (create-status-line
             (type: (optional num optional str -> void)))
           (on-set-focus (type: (-> void)))
           (on-activate (type: (bool -> void)))
           (get-title (type: (-> str)))
           (on-close (type: (-> bool)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (set-title (type: (str -> void)))
           (on-menu-command (type: (num -> void)))
           (set-icon (type: (wxicon-object -> void)))
           (on-menu-select (type: (num -> void))))))

(define wx:buffer-data-class-list%
  (class wx:object%
         ()
         (public
           (nth (type: (num -> (union null wxbufferdataclass-object))))
           (find-position
             (type: (wxbufferdataclass-object -> num)))
           (number (type: (-> num)))
           (find (type: (str -> (union null wxbufferdataclass-object))))
           (add (type: (wxbufferdataclass-object -> void))))))

(define wx:canvas-media-admin%
  (class wx:media-admin%
         ()
         (public
           (get-canvas (type: (-> wxmediacanvas-object))))))

(define wx:font-list%
  (class wx:object%
         ()
         (public
           (find-or-create-font
             (type: (union (num num num num optional bool -> wxfont-object)
                           (num str
                                num
                                num
                                num
                                optional
                                bool
                                ->
                                wxfont-object)))))))

(define wx:menu%
  (class wx:object%
         ((init1 (type: str))
          (init2 (type: (wxmenu-object wxevent-object -> void))))
         (public
           (get-help-string (type: (num -> str)))
           (checked? (type: (num -> bool)))
           (delete-by-position (type: (num -> void)))
           (set-help-string (type: (num str -> void)))
           (set-label (type: (num str -> void)))
           (append-separator (type: (-> void)))
           (get-title (type: (-> str)))
           (find-item (type: (str -> num)))
           (check (type: (num bool -> void)))
           (get-label (type: (num -> str)))
           (enable (type: (num bool -> void)))
           (set-title (type: (str -> void)))
           (delete (type: (num -> void)))
           (append
             (type: (union (num str wxmenu-object optional str -> void)
                           (num str optional str optional bool -> void)))))))

(define wx:choice%
  (class wx:item%
         (init1-arg-wxpanel-object
           |init2-arg-(wxchoice-object wxevent-object -> void)|
           init3-arg-str
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: (listof str)))
           (init9 (type: num))
           (init10 (type: str)))
         (public
           (get-string-selection (type: (-> str)))
           (set-columns (type: (optional num -> void)))
           (set-selection (type: (num -> void)))
           (get-columns (type: (-> num)))
           (clear (type: (-> void)))
           (find-string (type: (str -> num)))
           (set-string-selection (type: (str -> void)))
           (number (type: (-> num)))
           (get-selection (type: (-> num)))
           (get-string (type: (num -> str)))
           (append (type: (str -> void))))))

(define wx:keymap%
  (class wx:object%
         ()
         (public
           (handle-key-event
             (type: (wxobject-object wxkeyevent-object -> bool)))
           (set-double-click-interval (type: (num -> void)))
           (handle-mouse-event
             (type: (wxobject-object wxmouseevent-object -> bool)))
           (break-sequence (type: (-> void)))
           (remove-grab-key-function (type: (-> void)))
           (map-function (type: (str str -> void)))
           (implies-shift (type: (str -> void)))
           (remove-grab-mouse-function (type: (-> void)))
           (add-key-function
             (type: (str (wxobject-object wxkeyevent-object -> bool)
                         ->
                         void)))
           (set-grab-key-function
             (type: ((str wxkeymap-object
                          wxobject-object
                          wxkeyevent-object
                          ->
                          bool)
                     ->
                     void)))
           (set-break-sequence-callback
             (type: ((-> void) -> void)))
           (add-mouse-function
             (type: (str (wxobject-object wxmousrevent-object -> bool)
                         ->
                         void)))
           (set-grab-mouse-function
             (type: ((str wxkeymap-object
                          wxobject-object
                          wxmouseevent-object
                          ->
                          bool)
                     ->
                     void)))
           (call-function
             (type: (union (str wxobject-object
                                wxkeyevent-object
                                optional
                                bool
                                ->
                                bool)
                           (str wxobject-object
                                wxmouseevent-object
                                optional
                                bool
                                ->
                                bool))))
           (set-error-callback
             (type: ((str -> void) -> void)))
           (chain-to-keymap
             (type: (wxkeymap-object bool -> void)))
           (remove-chained-keymap
             (type: (wxkeymap-object -> void)))
           (get-double-click-interval (type: (-> num))))))

(define wx:style%
  (class wx:object%
         ()
         (public
           (get-colour (type: (-> wxcolour-object)))
           (get-family (type: (-> num)))
           (get-size (type: (-> num)))
           (get-style (type: (-> num)))
           (get-weight (type: (-> num)))
           (get-underlined (type: (-> bool)))
           (get-foreground (type: (-> wxcolour-object)))
           (get-alignment (type: (-> num)))
           (get-transparent-text-backing (type: (-> bool)))
           (get-base-style (type: (-> wxstyle-object)))
           (set-base-style (type: (wxstyle-object -> void)))
           (get-delta (type: (wxstyledelta-object -> void)))
           (get-name (type: (-> str)))
           (is-join? (type: (-> bool)))
           (get-shift-style (type: (-> wxstyle-object)))
           (get-font (type: (-> wxfont-object)))
           (set-shift-style
             (type: (wxstyle-object -> void)))
           (switch-to
             (type: (wxdc-object wxstyle-object -> void)))
           (get-face (type: (-> str)))
           (set-delta (type: (wxstyledelta-object -> void))))))

(define wx:canvas-dc%
  (class wx:dc%
         ()
         (public
           (begin-set-pixel (type: (-> void)))
           (end-set-pixel (type: (-> void)))
           (set-pixel
             (type: (num num (union null wxcolour-object) -> void)))
           (get-pixel
             (type: (num num (union null wxcolour-object) -> bool))))))

(define wx:command-event%
  (class wx:event%
         (init1-arg-num)
         (public
           (checked? (type: (-> bool)))
           (get-command-string
             (type: (union (str -> void) (-> str))))
           (is-selection? (type: (-> bool)))
           (get-command-int
             (type: (union (num -> void) (-> num))))
           (get-extra-long
             (type: (union (num -> void) (-> num))))
           (get-selection (type: (-> num)))
           (get-string (type: (-> str))))))

(define wx:snip-admin%
  (class wx:object%
         ()
         (public
           (release-snip (type: (wxsnip-object -> bool)))
           (get-dc (type: (-> wxdc-object)))
           (get-view
             (type: ((union null (box num))
                     (union null (box num))
                     (union null (box num))
                     (union null (box num))
                     optional
                     (union null wxsnip-object)
                     ->
                     void)))
           (set-cursor (type: (wxcursor-object -> void)))
           (resized (type: (wxsnip-object bool -> void)))
           (get-view-size
             (type: ((union null (box num))
                     (union null (box num))
                     ->
                     void)))
           (scroll-to
             (type: (wxsnip-object
                      num
                      num
                      num
                      num
                      bool
                      optional
                      num
                      ->
                      bool)))
           (set-caret-owner
             (type: (wxsnip-object optional num -> void)))
           (needs-update
             (type: (wxsnip-object num num num num -> void)))
           (get-media
             (type: (-> (union null wxmediabuffer-object)))))))

(define wx:snip-class-list%
  (class wx:object%
         ()
         (public
           (nth (type: (num -> (union null wxsnipclass-object))))
           (find-position
             (type: (wxsnipclass-object -> num)))
           (number (type: (-> num)))
           (find (type: (str -> (union null wxsnipclass-object))))
           (add (type: (wxsnipclass-object -> void))))))

(define wx:text-window%
  (class wx:window%
         (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: str)))
         (public
           (get-line-length (type: (num -> num)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (get-number-of-lines (type: (-> num)))
           (position-to-x-y
             (type: (num (box num) (box num) -> void)))
           (x-y-to-position (type: (num num -> num)))
           (load-file (type: (str -> bool)))
           (on-char (type: (wxkeyevent-object -> void)))
           (popup-menu
             (type: (wxmenu-object num num -> bool)))
           (set-insertion-point (type: (num -> void)))
           (set-selection (type: (num num -> void)))
           (set-insertion-point-end (type: (-> void)))
           (save-file (type: (str -> bool)))
           (show-position (type: (num -> void)))
           (set-font (type: (wxfont-object -> void)))
           (modified? (type: (-> bool)))
           (write-text (type: (str -> void)))
           (cut (type: (-> void)))
           (on-size (type: (num num -> void)))
           (clear (type: (-> void)))
           (discard-edits (type: (-> void)))
           (copy (type: (-> void)))
           (on-kill-focus (type: (-> void)))
           (paste (type: (-> void)))
           (replace (type: (num num str -> void)))
           (on-set-focus (type: (-> void)))
           (get-contents (type: (-> str)))
           (get-insertion-point (type: (-> num)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (get-last-position (type: (-> num)))
           (remove (type: (num num -> void))))))

(define wx:media-stream-out-string-base%
  (class wx:media-stream-out-base%
         ()
         (public (get-string (type: (-> str))))))

(define wx:post-script-dc%
  (class wx:dc%
         (init1-arg-str
           (init2 (type: bool))
           (init3 (type: (union null wxwindow-object))))
         (public)))

(define wx:style-delta%
  (class wx:object%
         ((init1 (type: num)) (init2 (type: num)))
         (public
           (get-family
             (type: (union (num -> void) (-> num))))
           (get-background-add
             (type: (-> wxaddcolour-object)))
           (set-delta-face
             (type: (str -> wxstyledelta-object)))
           (set-delta-background
             (type: (union (str -> wxstyledelta-object)
                           (wxcolour-object -> wxstyledelta-object))))
           (get-foreground-add
             (type: (-> wxaddcolour-object)))
           (set-delta-foreground
             (type: (union (str -> wxstyledelta-object)
                           (wxcolour-object -> wxstyledelta-object))))
           (get-transparent-text-backing-on
             (type: (union (bool -> void) (-> bool))))
           (collapse (type: (wxstyledelta-object -> bool)))
           (get-foreground-mult
             (type: (-> wxmultcolour-object)))
           (equal? (type: (wxstyledelta-object -> bool)))
           (get-underlined-off
             (type: (union (bool -> void) (-> bool))))
           (get-alignment-on
             (type: (union (num -> void) (-> num))))
           (get-underlined-on
             (type: (union (bool -> void) (-> bool))))
           (get-style-off
             (type: (union (num -> void) (-> num))))
           (get-style-on
             (type: (union (num -> void) (-> num))))
           (get-weight-off
             (type: (union (num -> void) (-> num))))
           (copy (type: (wxstyledelta-object -> void)))
           (get-weight-on
             (type: (union (num -> void) (-> num))))
           (get-size-add
             (type: (union (num -> void) (-> num))))
           (get-background-mult
             (type: (-> wxmultcolour-object)))
           (get-size-mult
             (type: (union (num -> void) (-> num))))
           (get-alignment-off
             (type: (union (num -> void) (-> num))))
           (get-face (type: (union (str -> void) (-> str))))
           (get-transparent-text-backing-off
             (type: (union (bool -> void) (-> bool))))
           (set-delta
             (type: (num optional num -> wxstyledelta-object))))))

(define wx:font-name-directory%
  (class wx:object%
         ()
         (public
           (get-family (type: (num -> num)))
           (get-afm-name (type: (num num num -> str)))
           (find-or-create-font-id (type: (str num -> num)))
           (get-font-id (type: (str -> num)))
           (get-new-font-id (type: (-> num)))
           (get-screen-name (type: (num num num -> str)))
           (initialize (type: (num num str -> void)))
           (get-post-script-name
             (type: (num num num -> str)))
           (get-font-name (type: (num -> str))))))

(define wx:button%
  (class wx:item%
         (init1-arg-wxpanel-object-or-init1-arg-wxpanel-object
           |init2-arg-(wxbutton-object wxevent-object -> void)-or-init2-arg-(wxbutton-object wxevent-object -> void)|
           init3-arg-str-or-init3-arg-wxbitmap-object
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: str)))
         (public
           (set-label
             (type: (union (wxbitmap-object -> void) (str -> void))))
           (set-default (type: (-> void))))))

(define wx:add-colour%
  (class wx:object%
         ()
         (public
           (get-b (type: (union (num -> void) (-> num))))
           (get-g (type: (union (num -> void) (-> num))))
           (get (type: ((box num) (box num) (box num) -> void)))
           (set (type: (num num num -> void)))
           (get-r (type: (union (num -> void) (-> num)))))))

(define wx:clipboard%
  (class wx:object%
         ()
         (public
           (set-clipboard-client
             (type: (wxclipboardclient-object num -> void)))
           (get-clipboard-data (type: (str num -> str)))
           (set-clipboard-string (type: (str num -> void)))
           (get-clipboard-client
             (type: (-> (union null wxclipboardclient-object))))
           (get-clipboard-string (type: (num -> str))))))

(define wx:icon%
  (class wx:bitmap%
         (init1-arg-str (init2 (type: num)))
         (public)))

(define wx:brush-list%
  (class wx:object%
         ()
         (public
           (find-or-create-brush
             (type: (union (wxcolour-object num -> wxbrush-object)
                           (str num -> wxbrush-object)))))))

(define wx:key-event%
  (class wx:event%
         (init1-arg-num)
         (public
           (get-alt-down
             (type: (union (bool -> void) (-> bool))))
           (get-key-code
             (type: (union (num -> void) (-> num))))
           (get-meta-down
             (type: (union (bool -> void) (-> bool))))
           (key-code (type: (-> num)))
           (get-control-down
             (type: (union (bool -> void) (-> bool))))
           (position (type: ((box num) (box num) -> void)))
           (get-time-stamp
             (type: (union (num -> void) (-> num))))
           (get-shift-down
             (type: (union (bool -> void) (-> bool)))))))

(define wx:media-snip-media-admin%
  (class wx:media-admin%
         ()
         (public
           (get-snip (type: (-> wxmediasnip-object))))))

(define wx:media-snip%
  (class wx:snip%
         ((init1 (type: (union null wxmediabuffer-object)))
          (init2 (type: bool))
          (init3 (type: num))
          (init4 (type: num))
          (init5 (type: num))
          (init6 (type: num))
          (init7 (type: num))
          (init8 (type: num))
          (init9 (type: num))
          (init10 (type: num))
          (init11 (type: num))
          (init12 (type: num))
          (init13 (type: num))
          (init14 (type: num)))
         (public
           (get-margin
             (type: ((box num) (box num) (box num) (box num) -> void)))
           (match? (type: (wxsnip-object -> bool)))
           (own-caret (type: (bool -> void)))
           (set-inset (type: (num num num num -> void)))
           (on-event
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxmouseevent-object
                      ->
                      void)))
           (get-extent
             (type: (wxdc-object
                      num
                      num
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      ->
                      void)))
           (size-cache-invalid (type: (-> void)))
           (get-inset
             (type: ((box num) (box num) (box num) (box num) -> void)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (get-text (type: (num num optional bool -> str)))
           (on-char
             (type: (wxdc-object
                      num
                      num
                      num
                      num
                      wxkeyevent-object
                      ->
                      void)))
           (get-max-width (type: (-> num)))
           (write (type: (wxmediastreamout-object -> void)))
           (get-min-width (type: (-> num)))
           (merge-with
             (type: (wxsnip-object -> wxsnip-object)))
           (set-max-width (type: (num -> void)))
           (set-admin (type: (wxsnipadmin-object -> void)))
           (do-font (type: (num optional bool -> void)))
           (set-min-width (type: (num -> void)))
           (partial-offset
             (type: (wxdc-object num num num -> num)))
           (get-max-height (type: (-> num)))
           (get-min-height (type: (-> num)))
           (split (type: (num (box wxsnip-object)
                              (box wxsnip-object)
                              ->
                              void)))
           (copy (type: (-> wxsnip-object)))
           (set-max-height (type: (num -> void)))
           (set-min-height (type: (num -> void)))
           (get-this-media
             (type: (-> (union null wxmediabuffer-object))))
           (show-border (type: (bool -> void)))
           (set-media
             (type: ((union null wxmediabuffer-object) -> void)))
           (resize (type: (num num -> bool)))
           (border-visible? (type: (-> bool)))
           (get-media
             (type: (-> (union null wxmediabuffer-object))))
           (draw (type: (wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (set-margin (type: (num num num num -> void))))))

(define wx:media-canvas%
  (class wx:canvas%
         (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
           (init2 (type: num))
           (init3 (type: num))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: str))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: (union null wxmediabuffer-object))))
         (public
           (is-focus-on? (type: (-> bool)))
           (force-display-focus (type: (bool -> void)))
           (on-paint (type: (-> void)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (allow-scroll-to-last (type: (bool -> void)))
           (on-event (type: (wxmouseevent-object -> void)))
           (scroll-with-bottom-base (type: (bool -> void)))
           (on-char (type: (wxkeyevent-object -> void)))
           (get-lazy-refresh (type: (-> bool)))
           (set-lazy-refresh (type: (bool -> void)))
           (on-size (type: (num num -> void)))
           (call-as-primary-owner (type: ((-> _) -> empty)))
           (on-kill-focus
             (type: (union (-> void) (-> void))))
           (on-set-focus
             (type: (union (-> void) (-> void))))
           (set-media
             (type: ((union null wxmediabuffer-object)
                     optional
                     bool
                     ->
                     void)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (get-media
             (type: (-> (union null wxmediabuffer-object)))))))

(define wx:meta-file-dc%
  (class wx:dc% ((init1 (type: str))) (public)))

(define wx:timer%
  (class wx:object%
         ()
         (public
           (notify (type: (-> void)))
           (start (type: (num optional bool -> bool)))
           (stop (type: (-> void)))
           (interval (type: (-> num))))))

(define wx:memory-dc%
  (class wx:canvas-dc%
         (init1-arg-wxcanvasdc-object)
         (public
           (select-object
             (type: ((union null wxbitmap-object) -> void))))))

(define wx:mouse-event%
  (class wx:event%
         (init1-arg-num)
         (public
           (get-right-down
             (type: (union (bool -> void) (-> bool))))
           (get-middle-down
             (type: (union (bool -> void) (-> bool))))
           (get-left-down
             (type: (union (bool -> void) (-> bool))))
           (button-d-click? (type: (optional num -> bool)))
           (button-down? (type: (optional num -> bool)))
           (get-time-stamp
             (type: (union (num -> void) (-> num))))
           (button-up? (type: (optional num -> bool)))
           (get-alt-down
             (type: (union (bool -> void) (-> bool))))
           (dragging? (type: (-> bool)))
           (get-meta-down
             (type: (union (bool -> void) (-> bool))))
           (entering? (type: (-> bool)))
           (get-control-down
             (type: (union (bool -> void) (-> bool))))
           (leaving? (type: (-> bool)))
           (get-shift-down
             (type: (union (bool -> void) (-> bool))))
           (is-button? (type: (-> bool)))
           (get-y (type: (union (num -> void) (-> num))))
           (get-x (type: (union (num -> void) (-> num))))
           (button? (type: (num -> bool)))
           (moving? (type: (-> bool))))))

(define wx:printer-dc%
  (class wx:dc%
         (init1-arg-str
           init2-arg-str
           init3-arg-str
           (init4 (type: bool)))
         (public)))

(define wx:buffer-data%
  (class wx:object%
         ()
         (public
           (get-next
             (type: (union ((union null wxbufferdata-object) -> void)
                           (-> (union null wxbufferdata-object)))))
           (write (type: (wxmediastreamout-object -> bool)))
           (get-dataclass
             (type: (union ((union null wxbufferdataclass-object) -> void)
                           (-> (union null wxbufferdataclass-object))))))))

(define wx:meta-file%
  (class wx:object%
         ()
         (public
           (set-clipboard
             (type: (optional num optional num -> bool)))
           (play (type: (wxdc-object -> void)))
           (ok? (type: (-> bool))))))

(define wx:text%
  (class wx:item%
         (init1-arg-wxpanel-object
           |init2-arg-(wxtext-object wxevent-object -> void)|
           init3-arg-str
           (init4 (type: str))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: num))
           (init10 (type: str)))
         (public
           (set-value (type: (str -> void)))
           (copy (type: (-> void)))
           (paste (type: (-> void)))
           (get-value (type: (-> str)))
           (on-char (type: (wxkeyevent-object -> void)))
           (set-editable (type: (bool -> void)))
           (cut (type: (-> void))))))

(define wx:slider%
  (class wx:item%
         (init1-arg-wxpanel-object
           |init2-arg-(wxslider-object wxevent-object -> void)|
           init3-arg-str
           init4-arg-num
           init5-arg-num
           init6-arg-num
           init7-arg-num
           (init8 (type: num))
           (init9 (type: num))
           (init10 (type: num))
           (init11 (type: str)))
         (public
           (set-value (type: (num -> void)))
           (get-value (type: (-> num))))))

(define wx:snip-class%
  (class wx:object%
         ()
         (public
           (read (type: (wxmediastreamin-object
                          ->
                          (union null wxsnip-object))))
           (get-classname
             (type: (union (str -> void) (-> str))))
           (write-header
             (type: (wxmediastreamout-object -> bool)))
           (read-header
             (type: (wxmediastreamin-object -> bool)))
           (write-done (type: (-> void)))
           (get-version
             (type: (union (num -> void) (-> num))))
           (read-done (type: (-> void))))))

(define wx:media-buffer%
  (class wx:object%
         ()
         (public
           (set-filename
             (type: (str optional bool -> void)))
           (set-min-height (type: (num -> void)))
           (needs-update
             (type: (wxsnip-object num num num num -> void)))
           (read-header-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (style-has-changed
             (type: ((union null wxstyle-object) -> void)))
           (print (type: (optional str optional bool -> void)))
           (begin-edit-sequence
             (type: (optional bool -> void)))
           (write-headers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (end-edit-sequence (type: (-> void)))
           (get-descent (type: (-> num)))
           (paste (type: (optional num -> void)))
           (write-footers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (get-snip-location
             (type: (wxsnip-object
                      optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      bool
                      ->
                      bool)))
           (write-to-file
             (type: (union (wxmediastreamout-object -> bool)
                           (wxmediastreamout-object -> bool))))
           (invalidate-bitmap-cache
             (type: (optional
                      num
                      optional
                      num
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (scroll-line-location (type: (num -> num)))
           (num-scroll-lines (type: (-> num)))
           (on-new-image-snip
             (type: (str num bool bool -> wximagesnip-object)))
           (find-scroll-line (type: (num -> num)))
           (resized (type: (wxsnip-object bool -> void)))
           (undo (type: (-> void)))
           (print-to-dc (type: (wxdc-object -> void)))
           (set-load-overwrites-styles
             (type: (bool -> void)))
           (do-edit
             (type: (num optional bool optional num -> void)))
           (get-admin
             (type: (-> (union null wxmediaadmin-object))))
           (do-font (type: (num optional bool -> void)))
           (lock (type: (bool -> void)))
           (get-load-overwrites-styles (type: (-> bool)))
           (on-load-file (type: (str num -> bool)))
           (set-admin
             (type: ((union null wxmediaadmin-object) -> void)))
           (get-snip-data
             (type: (wxsnip-object
                      ->
                      (union null wxbufferdata-object))))
           (after-load-file (type: (bool -> void)))
           (global-to-local
             (type: ((box num) (box num) -> void)))
           (begin-write-header-footer-to-file
             (type: (wxmediastreamout-object str (box num) -> bool)))
           (get-file (type: (str -> str)))
           (local-to-global
             (type: ((box num) (box num) -> void)))
           (set-caret-owner
             (type: ((union null wxsnip-object) optional num -> void)))
           (kill (type: (optional num -> void)))
           (put-file (type: (str str -> str)))
           (select-all (type: (-> void)))
           (end-write-header-footer-to-file
             (type: (wxmediastreamout-object num -> bool)))
           (get-flattened-text (type: (-> str)))
           (clear-undos (type: (-> void)))
           (set-max-undo-history (type: (num -> void)))
           (release-snip (type: (wxsnip-object -> bool)))
           (get-inactive-caret-threshold (type: (-> num)))
           (on-new-box (type: (num -> wxsnip-object)))
           (get-max-undo-history (type: (-> num)))
           (redo (type: (-> void)))
           (on-char (type: (wxkeyevent-object -> void)))
           (append-edit-items
             (type: (wxmenu-object optional num -> num)))
           (set-inactive-caret-threshold
             (type: (num -> void)))
           (scroll-to
             (type: (wxsnip-object num num num num bool -> bool)))
           (append-font-items
             (type: (wxmenu-object optional num -> num)))
           (read-footer-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (set-keymap
             (type: (optional (union null wxkeymap-object) -> void)))
           (get-buffer-type
             (type: (union (num -> void) (-> num))))
           (buffer-location-to-dc-location
             (type: (num num -> (box void))))
           (after-save-file (type: (bool -> void)))
           (own-caret (type: (bool -> void)))
           (get-keymap (type: (-> wxkeymap-object)))
           (copy-self (type: (-> wxmediabuffer-object)))
           (refresh (type: (num num num num bool -> void)))
           (size-cache-invalid (type: (-> void)))
           (add-buffer-functions
             (type: (wxkeymap-object -> void)))
           (get-extent
             (type: ((union null (box num))
                     (union null (box num))
                     ->
                     void)))
           (dc-location-to-buffer-location
             (type: (num num -> (box void))))
           (insert (type: (wxsnip-object -> void)))
           (get-dc (type: (-> wxdc-object)))
           (on-local-event
             (type: (wxmouseevent-object -> void)))
           (get-style-list (type: (-> wxstylelist-object)))
           (read-from-file
             (type: (union (wxmediastreamin-object optional bool -> bool)
                           (wxmediastreamin-object -> bool))))
           (on-paint
             (type: (bool wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (on-local-char
             (type: (wxkeyevent-object -> void)))
           (get-space (type: (-> num)))
           (cut (type: (optional bool optional num -> void)))
           (clear (type: (-> void)))
           (on-event (type: (wxmouseevent-object -> void)))
           (on-default-event
             (type: (wxmouseevent-object -> void)))
           (set-style-list
             (type: (wxstylelist-object -> void)))
           (get-max-width (type: (-> num)))
           (copy (type: (optional bool optional num -> void)))
           (on-default-char
             (type: (wxkeyevent-object -> void)))
           (modified? (type: (-> bool)))
           (get-min-width (type: (-> num)))
           (on-focus (type: (bool -> void)))
           (get-filename
             (type: (optional (union null (box bool)) -> str)))
           (set-max-width (type: (num -> void)))
           (on-change (type: (-> void)))
           (insert-box (type: (optional num -> void)))
           (set-min-width (type: (num -> void)))
           (get-view-size
             (type: ((union null (box num))
                     (union null (box num))
                     ->
                     void)))
           (insert-image
             (type: (optional
                      str
                      optional
                      num
                      optional
                      bool
                      optional
                      bool
                      ->
                      void)))
           (get-max-height (type: (-> num)))
           (change-style
             (type: ((union null wxstyledelta-object) -> void)))
           (on-save-file (type: (str num -> bool)))
           (set-snip-data
             (type: (wxsnip-object
                      (union null wxbufferdata-object)
                      ->
                      void)))
           (get-focus-snip
             (type: (-> (union null wxsnip-object))))
           (get-min-height (type: (-> num)))
           (set-modified (type: (bool -> void)))
           (set-max-height (type: (num -> void))))))

(define wx:buffer-data-class%
  (class wx:object%
         ()
         (public
           (read (type: (wxmediastreamin-object
                          ->
                          (union null wxbufferdata-object))))
           (get-classname
             (type: (union (str -> void) (-> str))))
           (get-required
             (type: (union (bool -> void) (-> bool)))))))

(define wx:media-stream-out%
  (class wx:object%
         (init1-arg-wxmediastreamoutbase-object)
         (public
           (jump-to (type: (num -> void)))
           (tell (type: (-> num)))
           (put (type: (union (num str -> wxmediastreamout-object)
                              (union (str -> wxmediastreamout-object)
                                     (union (num -> wxmediastreamout-object)
                                            (num ->
                                                 wxmediastreamout-object))))))
           (put-fixed
             (type: (num -> wxmediastreamout-object)))
           (<< (type: (union (str -> wxmediastreamout-object)
                             (union (num -> wxmediastreamout-object)
                                    (num -> wxmediastreamout-object)))))
           (ok? (type: (-> bool))))))

(define wx:multi-text%
  (class wx:text%
         (init1-arg-wxpanel-object
           |init2-arg-(wxmultitext-object wxevent-object -> void)|
           init3-arg-str
           (init4 (type: str))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: num))
           (init10 (type: str)))
         (public
           (get-value (type: (-> str)))
           (on-char (type: (wxkeyevent-object -> void))))))

(define wx:media-pasteboard%
  (class wx:media-buffer%
         ()
         (public
           (move (type: (union (wxsnip-object num num -> void)
                               (num num -> void))))
           (resize (type: (wxsnip-object num num -> bool)))
           (on-insert
             (type: (wxsnip-object
                      (union null wxsnip-object)
                      num
                      num
                      ->
                      bool)))
           (lower (type: (wxsnip-object -> void)))
           (release-snip (type: (wxsnip-object -> bool)))
           (on-load-file (type: (str num -> bool)))
           (read-header-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (after-insert
             (type: (wxsnip-object
                      (union null wxsnip-object)
                      num
                      num
                      ->
                      void)))
           (read-footer-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (after-load-file (type: (bool -> void)))
           (write-headers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (after-delete (type: (wxsnip-object -> void)))
           (get-file (type: (str -> str)))
           (set-selected (type: (wxsnip-object -> void)))
           (add-selected
             (type: (union (wxsnip-object -> void)
                           (num num num num -> void))))
           (put-file (type: (str str -> str)))
           (no-selected (type: (-> void)))
           (get-flattened-text (type: (-> str)))
           (on-new-image-snip
             (type: (str num bool bool -> wximagesnip-object)))
           (resized (type: (wxsnip-object bool -> void)))
           (get-center
             (type: ((box num) (box num) -> void)))
           (raise (type: (wxsnip-object -> void)))
           (delete
             (type: (union (-> void) (wxsnip-object -> void))))
           (find-first-snip
             (type: (-> (union null wxsnip-object))))
           (is-selected?
             (type: ((union null wxsnip-object) -> bool)))
           (refresh (type: (num num num num bool -> void)))
           (find-next-selected-snip
             (type: ((union null wxsnip-object)
                     ->
                     (union null wxsnip-object))))
           (erase (type: (-> void)))
           (on-move-to
             (type: (wxsnip-object num num bool -> bool)))
           (needs-update
             (type: (wxsnip-object num num num num -> void)))
           (copy (type: (optional bool optional num -> void)))
           (after-move-to
             (type: (wxsnip-object num num bool -> void)))
           (set-caret-owner
             (type: ((union null wxsnip-object) optional num -> void)))
           (kill (type: (optional num -> void)))
           (on-resize
             (type: (wxsnip-object num num -> bool)))
           (set-modified (type: (bool -> void)))
           (after-resize
             (type: (wxsnip-object num num bool -> void)))
           (on-select (type: (wxsnip-object bool -> bool)))
           (set-filename
             (type: (str optional bool -> void)))
           (after-select
             (type: (wxsnip-object bool -> void)))
           (remove (type: (wxsnip-object -> void)))
           (on-double-click
             (type: (wxsnip-object wxmouseevent-object -> void)))
           (find-snip
             (type: (num num -> (union null wxsnip-object))))
           (scroll-to
             (type: (wxsnip-object num num num num bool -> bool)))
           (add-pasteboard-functions
             (type: (wxkeymap-object -> void)))
           (interactive-adjust-move
             (type: (wxsnip-object (box num) (box num) -> void)))
           (set-before
             (type: (wxsnip-object
                      (union null wxsnip-object)
                      ->
                      void)))
           (on-delete (type: (wxsnip-object -> bool)))
           (on-interactive-move (type: (-> bool)))
           (on-paint
             (type: (bool wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (interactive-adjust-mouse
             (type: ((box num) (box num) -> void)))
           (set-after
             (type: (wxsnip-object
                      (union null wxsnip-object)
                      ->
                      void)))
           (after-interactive-move (type: (-> void)))
           (own-caret (type: (bool -> void)))
           (on-event (type: (wxmouseevent-object -> void)))
           (on-interactive-resize
             (type: (wxsnip-object -> bool)))
           (size-cache-invalid (type: (-> void)))
           (on-char (type: (wxkeyevent-object -> void)))
           (write-footers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (insert
             (type: (union (wxsnip-object -> void)
                           (union (wxsnip-object num num -> void)
                                  (union (wxsnip-object
                                           (union null wxsnip-object)
                                           ->
                                           void)
                                         (wxsnip-object
                                           (union null wxsnip-object)
                                           num
                                           num
                                           ->
                                           void))))))
           (interactive-adjust-resize
             (type: (wxsnip-object (box num) (box num) -> void)))
           (get-dragable (type: (-> bool)))
           (on-local-event
             (type: (wxmouseevent-object -> void)))
           (read-from-file
             (type: (wxmediastreamin-object optional bool -> bool)))
           (invalidate-bitmap-cache
             (type: (optional
                      num
                      optional
                      num
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (set-dragable (type: (bool -> void)))
           (on-local-char
             (type: (wxkeyevent-object -> void)))
           (write-to-file
             (type: (wxmediastreamout-object -> bool)))
           (cut (type: (optional bool optional num -> void)))
           (after-interactive-resize
             (type: (wxsnip-object -> void)))
           (get-selection-visible (type: (-> bool)))
           (on-default-event
             (type: (wxmouseevent-object -> void)))
           (load-file (type: (optional str -> bool)))
           (set-selection-visible (type: (bool -> void)))
           (on-default-char
             (type: (wxkeyevent-object -> void)))
           (paste (type: (optional num -> void)))
           (remove-selected (type: (wxsnip-object -> void)))
           (get-scroll-step (type: (-> num)))
           (on-focus (type: (bool -> void)))
           (do-paste (type: (num -> void)))
           (save-file (type: (optional str -> bool)))
           (set-scroll-step (type: (num -> void)))
           (on-change (type: (-> void)))
           (on-new-box (type: (num -> wxsnip-object)))
           (do-copy (type: (num bool -> void)))
           (get-snip-data
             (type: (wxsnip-object
                      ->
                      (union null wxbufferdata-object))))
           (change-style
             (type: (union ((union null wxstyledelta-object) -> void)
                           (union ((union null wxstyledelta-object)
                                   (union null wxsnip-object)
                                   ->
                                   void)
                                  ((union null wxstyle-object)
                                   optional
                                   (union null wxsnip-object)
                                   ->
                                   void)))))
           (on-save-file (type: (str num -> bool)))
           (set-snip-data
             (type: (wxsnip-object
                      (union null wxbufferdata-object)
                      ->
                      void)))
           (move-to (type: (wxsnip-object num num -> void)))
           (after-save-file (type: (bool -> void))))))

(define wx:media-edit%
  (class wx:media-buffer%
         ((init1 (type: num))
          (init2 (type: (listof num))))
         (public
           (find-line
             (type: (num optional (union null (box bool)) -> num)))
           (set-position-bias-scroll
             (type: (num num
                         optional
                         num
                         optional
                         bool
                         optional
                         bool
                         optional
                         num
                         ->
                         void)))
           (set-filename
             (type: (str optional bool -> void)))
           (on-insert (type: (num num -> bool)))
           (find-position-in-line
             (type: (num num
                         optional
                         (union null (box bool))
                         optional
                         (union null (box bool))
                         optional
                         (union null (box num))
                         ->
                         num)))
           (release-snip (type: (wxsnip-object -> bool)))
           (on-load-file (type: (str num -> bool)))
           (read-header-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (after-insert (type: (num num -> void)))
           (get-between-threshold (type: (-> num)))
           (erase (type: (-> void)))
           (read-footer-from-file
             (type: (wxmediastreamin-object str -> bool)))
           (on-delete (type: (num num -> bool)))
           (set-between-threshold (type: (num -> void)))
           (get-visible-position-range
             (type: ((union null (box num))
                     (union null (box num))
                     ->
                     void)))
           (write-headers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (after-delete (type: (num num -> void)))
           (paragraph-start-position
             (type: (num optional bool -> num)))
           (get-file (type: (str -> str)))
           (write-footers-to-file
             (type: (wxmediastreamout-object -> bool)))
           (on-change-style (type: (num num -> bool)))
           (position-location
             (type: (num optional
                         (union null (box num))
                         optional
                         (union null (box num))
                         optional
                         bool
                         optional
                         bool
                         optional
                         bool
                         ->
                         void)))
           (invalidate-bitmap-cache
             (type: (optional
                      num
                      optional
                      num
                      optional
                      num
                      optional
                      num
                      ->
                      void)))
           (after-change-style (type: (num num -> void)))
           (get-snip-position-and-location
             (type: (wxsnip-object
                      (union null (box num))
                      (union null (box num))
                      (union null (box num))
                      ->
                      void)))
           (position-line
             (type: (num optional bool -> num)))
           (on-edit-sequence (type: (-> void)))
           (line-start-position
             (type: (num optional bool -> num)))
           (on-new-image-snip
             (type: (str num bool bool -> wximagesnip-object)))
           (after-edit-sequence (type: (-> void)))
           (after-set-size-constraint (type: (-> void)))
           (find-string
             (type: (str optional
                         num
                         optional
                         num
                         optional
                         num
                         optional
                         bool
                         optional
                         bool
                         ->
                         num)))
           (on-new-box (type: (num -> wxsnip-object)))
           (after-set-position (type: (-> void)))
           (line-length (type: (num -> num)))
           (line-location
             (type: (num optional bool -> num)))
           (delete
             (type: (union (num optional num optional bool -> void)
                           (-> void))))
           (on-save-file (type: (str num -> bool)))
           (on-set-size-constraint (type: (-> bool)))
           (last-position (type: (-> num)))
           (after-save-file (type: (bool -> void)))
           (get-region-data
             (type: (num num -> (union null wxbufferdata-object))))
           (last-line (type: (-> num)))
           (refresh (type: (num num num num bool -> void)))
           (insert
             (type: (union (wxsnip-object -> void)
                           (union (str num optional num optional bool -> void)
                                  (union (str -> void)
                                         (union (num str
                                                     num
                                                     optional
                                                     num
                                                     optional
                                                     bool
                                                     ->
                                                     void)
                                                (union (num str -> void)
                                                       (union (wxsnip-object
                                                                num
                                                                optional
                                                                num
                                                                ->
                                                                void)
                                                              (union (char ->
                                                                           void)
                                                                     (char num
                                                                           optional
                                                                           num
                                                                           ->
                                                                           void))))))))))
           (position-paragraph
             (type: (num optional bool -> num)))
           (line-end-position
             (type: (num optional bool -> num)))
           (set-cursor
             (type: ((union null wxcursor-object) -> void)))
           (set-region-data
             (type: (num num
                         (union null wxbufferdata-object)
                         ->
                         void)))
           (paragraph-end-position
             (type: (num optional bool -> num)))
           (copy (type: (union (optional bool optional num -> void)
                               (bool num num optional num -> void))))
           (paste (type: (union (optional num -> void)
                                (num num optional num -> void))))
           (find-wordbreak
             (type: ((union null (box num))
                     (union null (box num))
                     num
                     ->
                     void)))
           (line-paragraph (type: (num -> num)))
           (kill (type: (union (optional num -> void)
                               (num num num -> void))))
           (do-copy (type: (num num num bool -> void)))
           (set-wordbreak-map
             (type: ((union null wxmediawordbreakmap-object) -> void)))
           (paragraph-start-line (type: (num -> num)))
           (after-load-file (type: (bool -> void)))
           (get-flattened-text (type: (-> str)))
           (get-wordbreak-map
             (type: (-> (union null wxmediawordbreakmap-object))))
           (pargraph-end-line (type: (num -> num)))
           (get-position
             (type: ((union null (box num))
                     optional
                     (union null (box num))
                     ->
                     void)))
           (hide-caret (type: (bool -> void)))
           (last-paragraph (type: (-> num)))
           (get-start-position (type: (-> num)))
           (find-position
             (type: (num num
                         optional
                         (union null (box bool))
                         optional
                         (union null (box bool))
                         optional
                         (union null (box num))
                         ->
                         num)))
           (caret-hidden? (type: (-> bool)))
           (find-string-all
             (type: (str optional
                         num
                         optional
                         num
                         optional
                         num
                         optional
                         bool
                         optional
                         bool
                         ->
                         (listof num))))
           (get-end-position (type: (-> num)))
           (on-char (type: (wxkeyevent-object -> void)))
           (find-snip
             (type: (num num
                         optional
                         (union null (box num))
                         ->
                         (union null wxsnip-object))))
           (set-position
             (type: (num optional
                         num
                         optional
                         bool
                         optional
                         bool
                         optional
                         num
                         ->
                         void)))
           (on-new-text-snip (type: (-> wxtextsnip-object)))
           (get-snip-position
             (type: (wxsnip-object -> num)))
           (move-position
             (type: (num optional bool optional num -> void)))
           (get-text
             (type: (optional
                      num
                      optional
                      num
                      optional
                      bool
                      optional
                      bool
                      ->
                      str)))
           (scroll-to-position
             (type: (num optional
                         bool
                         optional
                         num
                         optional
                         num
                         ->
                         bool)))
           (own-caret (type: (bool -> void)))
           (on-new-tab-snip (type: (-> wxtabsnip-object)))
           (get-character (type: (num -> char)))
           (get-visible-line-range
             (type: ((union null (box num))
                     (union null (box num))
                     ->
                     void)))
           (size-cache-invalid (type: (-> void)))
           (set-autowrap-bitmap
             (type: ((union null wxbitmap-object)
                     ->
                     (union null wxbitmap-object))))
           (insert-file (type: (str optional num -> bool)))
           (set-anchor (type: (bool -> void)))
           (needs-update
             (type: (wxsnip-object num num num num -> void)))
           (on-local-event
             (type: (wxmouseevent-object -> void)))
           (set-wordbreak-func
             (type: ((wxmediaedit-object
                       (box num)
                       (box num)
                       num
                       ->
                       void)
                     ->
                     void)))
           (read-from-file
             (type: (union (wxmediastreamin-object optional bool -> bool)
                           (wxmediastreamin-object
                             num
                             optional
                             bool
                             ->
                             bool))))
           (get-anchor (type: (-> bool)))
           (on-paint
             (type: (bool wxdc-object
                          num
                          num
                          num
                          num
                          num
                          num
                          num
                          ->
                          void)))
           (on-local-char
             (type: (wxkeyevent-object -> void)))
           (write-to-file
             (type: (union (wxmediastreamout-object -> bool)
                           (wxmediastreamout-object
                             num
                             optional
                             num
                             ->
                             bool))))
           (flash-on
             (type: (num num
                         optional
                         bool
                         optional
                         bool
                         optional
                         num
                         ->
                         void)))
           (cut (type: (union (optional bool optional num -> void)
                              (bool num num optional num -> void))))
           (on-change (type: (-> void)))
           (on-event (type: (wxmouseevent-object -> void)))
           (on-default-event
             (type: (wxmouseevent-object -> void)))
           (get-file-format (type: (-> num)))
           (flash-off (type: (-> void)))
           (load-file
             (type: (optional str optional num -> bool)))
           (on-default-char
             (type: (wxkeyevent-object -> void)))
           (set-clickback
             (type: (num num
                         (wxmediaedit-object num num -> void)
                         optional
                         (union null wxstyledelta-object)
                         optional
                         bool
                         ->
                         void)))
           (set-file-format (type: (num -> void)))
           (paste-next (type: (-> void)))
           (on-focus (type: (bool -> void)))
           (remove-clickback (type: (num num -> void)))
           (get-overwrite-mode (type: (-> bool)))
           (do-paste (type: (num num -> void)))
           (save-file
             (type: (optional str optional num -> bool)))
           (resized (type: (wxsnip-object bool -> void)))
           (set-overwrite-mode (type: (bool -> void)))
           (put-file (type: (str str -> str)))
           (set-caret-owner
             (type: ((union null wxsnip-object) optional num -> void)))
           (get-snip-data
             (type: (wxsnip-object
                      ->
                      (union null wxbufferdata-object))))
           (get-tabs
             (type: (optional
                      (union null (box num))
                      optional
                      (union null (box num))
                      optional
                      (union null (box bool))
                      ->
                      (listof num))))
           (change-style
             (type: (union ((union null wxstyledelta-object) -> void)
                           (union ((union null wxstyledelta-object)
                                   num
                                   optional
                                   num
                                   ->
                                   void)
                                  ((union null wxstyle-object)
                                   optional
                                   num
                                   optional
                                   num
                                   ->
                                   void)))))
           (scroll-to
             (type: (wxsnip-object num num num num bool -> bool)))
           (set-snip-data
             (type: (wxsnip-object
                      (union null wxbufferdata-object)
                      ->
                      void)))
           (set-tabs
             (type: ((listof num) optional num optional bool -> void)))
           (split-snip (type: (num -> void)))
           (set-modified (type: (bool -> void)))
           (add-editor-functions
             (type: (wxkeymap-object -> void))))))

(define wx:radio-box%
  (class wx:item%
         (init1-arg-wxpanel-object-or-init1-arg-wxpanel-object
           |init2-arg-(wxradiobox-object wxevent-object -> void)-or-init2-arg-(wxradiobox-object wxevent-object -> void)|
           init3-arg-str-or-init3-arg-str
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: (listof str)))
           (init9 (type: num))
           (init10 (type: num))
           (init11 (type: str)))
         (public
           (get-string-selection (type: (-> str)))
           (set-selection (type: (num -> void)))
           (find-string (type: (str -> num)))
           (set-string-selection (type: (str -> void)))
           (number (type: (-> num)))
           (enable
             (type: (union (num bool -> void) (bool -> void))))
           (get-selection (type: (-> num)))
           (get-string (type: (num -> str))))))

(define wx:dialog-box%
  (class wx:panel%
         (|init1-arg-(union null wxwindow-object)|
           init2-arg-str
           (init3 (type: bool))
           (init4 (type: num))
           (init5 (type: num))
           (init6 (type: num))
           (init7 (type: num))
           (init8 (type: num))
           (init9 (type: str)))
         (public
           (on-set-focus (type: (-> void)))
           (on-paint (type: (-> void)))
           (on-size (type: (num num -> void)))
           (pre-on-event
             (type: (wxwindow-object wxmouseevent-object -> bool)))
           (on-event (type: (wxmouseevent-object -> void)))
           (pre-on-char
             (type: (wxwindow-object wxkeyevent-object -> bool)))
           (on-close (type: (-> bool)))
           (on-kill-focus (type: (-> void)))
           (on-char (type: (wxkeyevent-object -> void)))
           (on-default-action
             (type: (wxitem-object -> void)))
           (on-activate (type: (bool -> void))))))


)
