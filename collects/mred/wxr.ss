(unit/sig mred:wx^ (import)

 (define union (lambda (a b c) (or a b c)))

 (define wx:const-k-numpad9 (type: num))
 (define wx:const-msnipbox-xmargin (type: num))
 (define wx:const-needed-sb (type: num))
 (define wx:const-stipple (type: num))
 (define wx:const-k-numpad8 (type: num))
 (define wx:const-event-type-set-focus
  (type: num))
 (define wx:const-vertical-hatch (type: num))
 (define wx:const-k-numpad7 (type: num))
 (define wx:const-sb-mask (type: num))
 (define wx:const-cross-hatch (type: num))
 (define wx:const-k-numpad6 (type: num))
 (define wx:const-event-type-text-enter-command
  (type: num))
 (define wx:const-snip-uses-buffer-path
  (type: num))
 (define wx:const-extended (type: num))
 (define wx:const-mcanvas-hide-v-scroll
  (type: num))
 (define wx:const-solid (type: num))
 (define wx:const-k-numpad5 (type: num))
 (define wx:const-snip-height-depends-on-y
  (type: num))
 (define wx:the-clipboard
  (type: (union null wxclipboard-object)))
 (define wx:const-transparent (type: num))
 (define wx:const-k-numpad4 (type: num))
 (define wx:const-event-type-radiobox-command
  (type: num))
 (define wx:const-mcanvas-hide-h-scroll
  (type: num))
 (define wx:const-k-numpad3 (type: num))
 (define wx:const-snip-width-depends-on-y
  (type: num))
 (define wx:const-single (type: num))
 (define wx:const-k-numpad2 (type: num))
 (define wx:const-event-type-slider-command
  (type: num))
 (define wx:const-mcanvas-no-v-scroll (type: num))
 (define wx:const-k-numpad1 (type: num))
 (define wx:const-multiple-mask (type: num))
 (define wx:const-k-numpad0 (type: num))
 (define wx:const-event-type-menu-command
  (type: num))
 (define wx:const-mcanvas-no-h-scroll (type: num))
 (define wx:const-k-help (type: num))
 (define wx:const-snip-width-depends-on-x
  (type: num))
 (define wx:const-k-insert (type: num))
 (define wx:const-event-type-multitext-command
  (type: num))
 (define wx:const-k-snapshot (type: num))
 (define wx:const-snip-handles-events (type: num))
 (define wx:const-snip-height-depends-on-x
  (type: num))
 (define wx:const-k-execute (type: num))
 (define wx:const-event-type-text-command
  (type: num))
 (define wx:the-style-list
  (type: wxstylelist-object))
 (define wx:const-overwrite-prompt (type: num))
 (define wx:const-k-print (type: num))
 (define wx:const-snip-hard-newline (type: num))
 (define wx:the-colour-database
  (type: wxcolourdatabase-object))
 (define wx:const-k-select (type: num))
 (define wx:const-event-type-listbox-command
  (type: num))
 (define wx:const-icon-information (type: num))
 (define wx:const-k-down (type: num))
 (define wx:const-snip-can-append (type: num))
 (define wx:const-k-right (type: num))
 (define wx:const-event-type-choice-command
  (type: num))
 (define wx:const-icon-exclamation (type: num))
 (define wx:const-k-up (type: num))
 (define wx:const-k-left (type: num))
 (define wx:const-event-type-checkbox-command
  (type: num))
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
 (define wx:const-k-home (type: num))
 (define wx:hourglass-cursor
  (type: wxcursor-object))
 (define wx:const-k-end (type: num))
 (define wx:const-event-type-button-command
  (type: num))
 (define wx:const-hide-readonly (type: num))
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
 (define wx:the-font-list
  (type: wxfontlist-object))
 (define wx:const-k-next (type: num))
 (define wx:const-save (type: num))
 (define wx:const-k-prior (type: num))
 (define wx:const-type-mouse-event (type: num))
 (define wx:const-open (type: num))
 (define wx:make-meta-file-placeable
  (type: (str num num num num num -> bool)))
 (define wx:const-k-capital (type: num))
 (define wx:const-dir (type: num))
 (define wx:const-k-pause (type: num))
 (define wx:const-type-command-event (type: num))
 (define wx:const-file (type: num))
 (define wx:get-print-preview-command
  (type: (-> str)))
 (define wx:const-slant (type: num))
 (define wx:const-k-menu (type: num))
 (define wx:const-default-dialog-style
  (type: num))
 (define wx:const-centre (type: num))
 (define wx:const-italic (type: num))
 (define wx:const-k-control (type: num))
 (define wx:const-event-type-scroll-thumbtrack
  (type: num))
 (define wx:const-center (type: num))
 (define wx:get-printer-orientation
  (type: (-> num)))
 (define wx:const-bold (type: num))
 (define wx:const-k-shift (type: num))
 (define wx:const-icon-mask (type: num))
 (define wx:const-light (type: num))
 (define wx:const-k-clear (type: num))
 (define wx:const-event-type-scroll-pagedown
  (type: num))
 (define wx:const-icon-asterisk (type: num))
 (define wx:get-printer-translation
  (type: ((box num) (box num) -> void)))
 (define wx:const-normal (type: num))
 (define wx:const-k-mbutton (type: num))
 (define wx:const-icon-stop (type: num))
 (define wx:const-teletype (type: num))
 (define wx:const-k-cancel (type: num))
 (define wx:const-event-type-scroll-pageup
  (type: num))
 (define wx:const-icon-question (type: num))
 (define wx:set-printer-preview-command
  (type: (str -> void)))
 (define wx:const-modern (type: num))
 (define wx:const-k-rbutton (type: num))
 (define wx:const-icon-hand (type: num))
 (define wx:const-swiss (type: num))
 (define wx:const-k-lbutton (type: num))
 (define wx:const-event-type-scroll-linedown
  (type: num))
 (define wx:const-align-center (type: num))
 (define wx:const-no (type: num))
 (define wx:set-printer-orientation
  (type: (num -> void)))
 (define wx:const-script (type: num))
 (define wx:const-k-start (type: num))
 (define wx:const-align-bottom (type: num))
 (define wx:const-yes (type: num))
 (define wx:const-roman (type: num))
 (define wx:const-k-delete (type: num))
 (define wx:const-event-type-scroll-lineup
  (type: num))
 (define wx:const-edit-buffer (type: num))
 (define wx:const-cancel (type: num))
 (define wx:set-printer-translation
  (type: (num num -> void)))
 (define wx:const-decorative (type: num))
 (define wx:const-k-space (type: num))
 (define wx:const-change-family (type: num))
 (define wx:const-yes-no (type: num))
 (define wx:const-default (type: num))
 (define wx:const-k-escape (type: num))
 (define wx:const-event-type-scroll-bottom
  (type: num))
 (define wx:const-change-smaller (type: num))
 (define wx:const-ok (type: num))
 (define wx:const-k-return (type: num))
 (define wx:const-k-tab (type: num))
 (define wx:const-change-bigger (type: num))
 (define wx:concat-files
  (type: (str str str -> bool)))
 (define wx:const-k-back (type: num))
 (define wx:const-change-italic (type: num))
 (define wx:copy-file (type: (str str -> bool)))
 (define wx:the-font-name-directory
  (type: wxfontnamedirectory-object))
 (define wx:const-cursor-spraycan (type: num))
 (define wx:const-change-bold (type: num))
 (define wx:get-temp-file-name
  (type: (str -> str)))
 (define wx:get-host-name (type: (-> str)))
 (define wx:const-change-size (type: num))
 (define wx:get-email-address (type: (-> str)))
 (define wx:const-cursor-sizenwse (type: num))
 (define wx:const-change-weight (type: num))
 (define wx:get-user-id (type: (-> str)))
 (define wx:const-change-style (type: num))
 (define wx:get-user-name (type: (-> str)))
 (define wx:const-cursor-sizenesw (type: num))
 (define wx:const-change-normal (type: num))
 (define wx:get-media-print-margin
  (type: (optional
	  (union null (box num))
	  optional
	  (union null (box num))
	  ->
	  void)))
 (define wx:string-match?
  (type: (str str optional bool optional bool -> bool)))
 (define wx:const-change-nothing (type: num))
 (define wx:set-display (type: (str -> bool)))
 (define wx:const-cursor-right-button (type: num))
 (define wx:const-base (type: num))
 (define wx:set-media-print-margin
  (type: (optional num optional num -> void)))
 (define wx:get-display-name (type: (-> str)))
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
 (define wx:const-cursor-question-arrow
  (type: num))
 (define wx:read-media-global-header
  (type: (wxmediastreamin-object -> bool)))
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
 (define wx:const-vertical (type: num))
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
 (define wx:const-horizontal (type: num))
 (define wx:const-cursor-point-right (type: num))
 (define wx:const-bitmap-type-bmp (type: num))
 (define wx:read-media-global-footer
  (type: (wxmediastreamin-object -> bool)))
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
 (define wx:const-both (type: num))
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
 (define wx:const-default-frame (type: num))
 (define wx:const-cursor-point-left (type: num))
 (define wx:write-media-global-header
  (type: (wxmediastreamout-object -> bool)))
 (define wx:colour-display? (type: (-> bool)))
 (define wx:const-resize-border (type: num))
 (define wx:display-depth (type: (-> num)))
 (define wx:const-resize-box (type: num))
 (define wx:const-cursor-painr-brush (type: num))
 (define wx:exit (type: (-> void)))
 (define wx:write-media-global-footer
  (type: (wxmediastreamout-object -> bool)))
 (define wx:set-cursor
  (type: (wxcursor-object -> void)))
 (define wx:const-maximize-box (type: num))
 (define wx:const-type-key-event (type: num))
 (define wx:get-printer-command (type: (-> str)))
 (define wx:const-minimize-box (type: num))
 (define wx:const-cursor-no-entry (type: num))
 (define wx:add-media-buffer-functions
  (type: (wxkeymap-object -> void)))
 (define wx:get-printer-file (type: (-> str)))
 (define wx:const-system-menu (type: num))
 (define wx:const-snip-anchored (type: num))
 (define wx:get-printer-mode (type: (-> num)))
 (define wx:const-thick-frame (type: num))
 (define wx:const-cursor-middle-button
  (type: num))
 (define wx:const-snip-invisible (type: num))
 (define wx:add-media-editor-functions
  (type: (wxkeymap-object -> void)))
 (define wx:get-printer-options (type: (-> str)))
 (define wx:const-mdi-child (type: num))
 (define wx:const-snip-is-text (type: num))
 (define wx:get-printer-scaling
  (type: ((box num) (box num) -> void)))
 (define wx:const-mdi-parent (type: num))
 (define wx:const-cursor-magnifier (type: num))
 (define wx:const-snip-newline (type: num))
 (define wx:add-media-pasteboard-functions
  (type: (wxkeymap-object -> void)))
 (define wx:get-print-paper-name (type: (-> str)))
 (define wx:const-sdi (type: num))
 (define wx:get-afm-path (type: (-> str)))
 (define wx:const-maximize (type: num))
 (define wx:const-cursor-left-button (type: num))
 (define wx:media-set-x-selection-mode
  (type: (bool -> void)))
 (define wx:set-printer-command
  (type: (str -> void)))
 (define wx:const-minimize (type: num))
 (define wx:set-printer-file
  (type: (str -> void)))
 (define wx:const-iconize (type: num))
 (define wx:const-cursor-bullseye (type: num))
 (define wx:get-the-snip-class-list
  (type: (-> wxsnipclasslist-object)))
 (define wx:set-printer-mode
  (type: (num -> void)))
 (define wx:const-stay-on-top (type: num))
 (define wx:set-printer-options
  (type: (str -> void)))
 (define wx:set-printer-scaling
  (type: (num num -> void)))
 (define wx:set-print-paper-name
  (type: (str -> void)))
 (define wx:const-horizontal-hatch (type: num))
 (define wx:get-the-buffer-data-class-list
  (type: (-> wxbufferdataclasslist-object)))
 (define wx:set-afm-path (type: (str -> void)))
 (define wx:new-id (type: (-> num)))
 (define wx:const-fdiagonal-hatch (type: num))
 (define wx:const-pasteboard-buffer (type: num))
 (define wx:register-id (type: (num -> void)))
 (define wx:const-crossdiag-hatch (type: num))
 (define wx:begin-busy-cursor
  (type: (optional wxcursor-object -> void)))
 (define wx:const-mm-text (type: num))
 (define wx:is-busy? (type: (-> bool)))
 (define wx:const-bdiagonal-hatch (type: num))
 (define wx:const-mm-lometric (type: num))
 (define wx:end-busy-cursor (type: (-> void)))
 (define wx:const-hscroll (type: num))
 (define wx:const-mm-metric (type: num))
 (define wx:display-size
  (type: ((box num) (box num) -> void)))
 (define wx:const-mm-points (type: num))
 (define wx:find-window-by-label
  (type: (str optional
	  (union null wxwindow-object)
	  ->
	  (union null wxwindow-object))))
 (define wx:const-mm-twips (type: num))
 (define wx:find-window-by-name
  (type: (str optional
	  (union null wxwindow-object)
	  ->
	  (union null wxwindow-object))))
 (define wx:const-xor (type: num))
 (define wx:const-local-select (type: num))
 (define wx:strip-menu-codes (type: (str -> str)))
 (define wx:const-src-invert (type: num))
 (define wx:const-x-select (type: num))
 (define wx:get-free-memory (type: (-> num)))
 (define wx:const-password (type: num))
 (define wx:const-set (type: num))
 (define wx:const-default-select (type: num))
 (define wx:const-break-for-user-2 (type: num))
 (define wx:get-resource
  (type: (union (str str (box str) optional str -> bool)
	  (str str (box num) optional str -> bool))))
 (define wx:const-or-reverse (type: num))
 (define wx:const-break-for-line (type: num))
 (define wx:write-resource
  (type: (union (str str str optional str -> bool)
	  (str str num optional str -> bool))))
 (define wx:const-or-invert (type: num))
 (define wx:const-snip-after (type: num))
 (define wx:const-break-for-user-1 (type: num))
 (define wx:bell (type: (-> void)))
 (define wx:const-pos-use-minus-one (type: num))
 (define wx:const-or (type: num))
 (define wx:const-snip-before (type: num))
 (define wx:yield
  (type: (optional semaphore -> bool)))
 (define wx:const-no-op (type: num))
 (define wx:const-media-ff-copy (type: num))
 (define wx:const-break-for-selection (type: num))
 (define wx:flush-display (type: (-> void)))
 (define duplicate-key-event
  (type: (wxkeyevent-object -> wxkeyevent-object)))
 (define wx:const-size-use-exsiting (type: num))
 (define wx:const-nor (type: num))
 (define wx:const-media-ff-same (type: num))
 (define duplicate-mouse-event
  (type: (wxmouseevent-object -> wxmouseevent-object)))
 (define wx:const-nand (type: num))
 (define wx:const-media-ff-text (type: num))
 (define wx:const-break-for-caret (type: num))
 (define wx:const-size-auto-height (type: num))
 (define wx:const-invert (type: num))
 (define wx:const-media-ff-std (type: num))
 (define wx:const-equiv (type: num))
 (define wx:const-media-ff-guess (type: num))
 (define wx:const-snip-after-or-null (type: num))
 (define wx:const-size-auto-width (type: num))
 (define wx:const-copy (type: num))
 (define wx:const-move-word (type: num))
 (define wx:const-allow-auto-resize (type: num))
 (define wx:const-clear (type: num))
 (define wx:const-move-page (type: num))
 (define wx:const-snip-before-or-null (type: num))
 (define wx:const-and-reverse (type: num))
 (define wx:const-move-line (type: num))
 (define wx:const-tiny-caption-vert (type: num))
 (define wx:const-and-invert (type: num))
 (define wx:const-move-simple (type: num))
 (define wx:const-snip-draw-show-inactive-caret
  (type: num))
 (define wx:const-and (type: num))
 (define wx:const-edit-kill (type: num))
 (define wx:const-tiny-caption-horiz (type: num))
 (define wx:const-winding-rule (type: num))
 (define wx:const-edit-paste (type: num))
 (define wx:const-snip-draw-show-caret
  (type: num))
 (define wx:const-cursor-watch (type: num))
 (define wx:const-multiple (type: num))
 (define wx:const-oddeven-rule (type: num))
 (define wx:const-edit-copy (type: num))
 (define wx:const-cursor-wait (type: num))
 (define wx:const-edit-cut (type: num))
 (define wx:const-snip-draw-no-caret (type: num))
 (define wx:const-cursor-sizing (type: num))
 (define wx:const-edit-clear (type: num))
 (define wx:const-cursor-sizewe (type: num))
 (define wx:const-k-numlock (type: num))
 (define wx:const-event-type-right-dclick
  (type: num))
 (define wx:const-edit-redo (type: num))
 (define wx:const-media-ff-text-force-cr
  (type: num))
 (define wx:const-cursor-sizens (type: num))
 (define wx:const-k-f24 (type: num))
 (define wx:const-edit-undo (type: num))
 (define wx:const-cursor-pencil (type: num))
 (define wx:const-k-f23 (type: num))
 (define wx:const-event-type-middle-dclick
  (type: num))
 (define wx:const-edit-select-all (type: num))
 (define wx:const-cursor-ibeam (type: num))
 (define wx:const-k-f22 (type: num))
 (define wx:const-cursor-hand (type: num))
 (define wx:const-k-f21 (type: num))
 (define wx:const-event-type-left-dclick
  (type: num))
 (define wx:const-edit-insert-image (type: num))
 (define wx:const-cursor-cross (type: num))
 (define wx:const-k-f20 (type: num))
 (define wx:const-change-normal-colour
  (type: num))
 (define wx:const-cursor-char (type: num))
 (define wx:const-k-f19 (type: num))
 (define wx:const-event-type-leave-window
  (type: num))
 (define wx:const-size-auto (type: num))
 (define wx:const-edit-insert-graphic-box
  (type: num))
 (define wx:const-cursor-arrow (type: num))
 (define wx:const-k-f18 (type: num))
 (define wx:const-change-alignment (type: num))
 (define wx:const-k-f17 (type: num))
 (define wx:const-event-type-enter-window
  (type: num))
 (define wx:const-edit-insert-text-box
  (type: num))
 (define wx:const-k-f16 (type: num))
 (define wx:const-change-toggle-underline
  (type: num))
 (define wx:const-bitmap-type-pict-resource
  (type: num))
 (define wx:the-pen-list (type: wxpenlist-object))
 (define wx:const-k-f15 (type: num))
 (define wx:const-event-type-motion (type: num))
 (define wx:const-k-f14 (type: num))
 (define wx:const-change-toggle-weight
  (type: num))
 (define wx:const-bitmap-type-pict (type: num))
 (define wx:const-k-f13 (type: num))
 (define wx:const-event-type-right-up (type: num))
 (define wx:const-k-f12 (type: num))
 (define wx:const-change-toggle-style (type: num))
 (define wx:const-bitmap-type-xpm (type: num))
 (define wx:const-dot-dash (type: num))
 (define wx:const-k-f11 (type: num))
 (define wx:const-event-type-right-down
  (type: num))
 (define wx:const-short-dash (type: num))
 (define wx:const-k-f10 (type: num))
 (define wx:const-change-underline (type: num))
 (define wx:const-bitmap-type-xbm (type: num))
 (define wx:const-long-dash (type: num))
 (define wx:const-k-f9 (type: num))
 (define wx:const-event-type-middle-up
  (type: num))
 (define wx:const-msnipbox-xinset (type: num))
 (define wx:const-dot (type: num))
 (define wx:const-k-f8 (type: num))
 (define wx:const-bitmap-type-gif (type: num))
 (define wx:const-cap-butt (type: num))
 (define wx:const-k-f7 (type: num))
 (define wx:const-event-type-middle-down
  (type: num))
 (define wx:const-cap-projecting (type: num))
 (define wx:const-k-f6 (type: num))
 (define wx:const-bitmap-type-bmp-resource
  (type: num))
 (define wx:const-readonly (type: num))
 (define wx:const-align-top (type: num))
 (define wx:const-cap-round (type: num))
 (define wx:const-k-f5 (type: num))
 (define wx:const-event-type-left-up (type: num))
 (define wx:const-backingstore (type: num))
 (define wx:const-join-round (type: num))
 (define wx:const-k-f4 (type: num))
 (define wx:const-editable (type: num))
 (define wx:const-retained (type: num))
 (define wx:const-join-miter (type: num))
 (define wx:const-k-f3 (type: num))
 (define wx:const-event-type-left-down
  (type: num))
 (define wx:const-border (type: num))
 (define wx:const-caption (type: num))
 (define wx:the-media-wordbreak-map
  (type: wxmediawordbreakmap-object))
 (define wx:const-join-bevel (type: num))
 (define wx:const-k-f2 (type: num))
 (define wx:const-bitmap-discard-colourmap
  (type: num))
 (define wx:const-k-f1 (type: num))
 (define wx:const-event-type-char (type: num))
 (define wx:const-k-divide (type: num))
 (define wx:const-msnipbox-yinset (type: num))
 (define wx:const-bitmap-type-default (type: num))
 (define wx:const-vscroll (type: num))
 (define wx:const-k-decimal (type: num))
 (define wx:const-event-type-virt-listbox-command
  (type: num))
 (define wx:const-k-subtract (type: num))
 (define wx:const-event-type-scroll-top
  (type: num))
 (define wx:the-brush-list
  (type: wxbrushlist-object))
 (define wx:const-k-separator (type: num))
 (define wx:const-event-type-scrollbar-command
  (type: num))
 (define wx:const-process-enter (type: num))
 (define wx:const-k-add (type: num))
 (define wx:const-msnipbox-ymargin (type: num))
 (define wx:const-always-sb (type: num))
 (define wx:const-k-multiply (type: num))
 (define wx:const-event-type-kill-focus
  (type: num))

 (define wx:object%
  (wxr:class* this ((super null)) () (public)))

 (define wx:dc%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (destroy-clipping-region (type: (-> void)))
    (get-logical-function (type: (-> num)))
    (set-clipping-region
     (type: (num num num num -> void)))
    (set-pen (type: (wxpen-object -> void)))
    (set-background-mode (type: (num -> void)))
    (get-map-mode (type: (-> num)))
    (clear (type: (-> void)))
    (get-clipping-region
     (type: ((box num) (box num) (box num) (box num) -> void)))
    (get-text-background
     (type: (-> wxcolour-object)))
    (begin-drawing (type: (-> void)))
    (set-background (type: (wxbrush-object -> void)))
    (get-text-foreground
     (type: (-> wxcolour-object)))
    (end-drawing (type: (-> void)))
    (get-size (type: ((box num) (box num) -> void)))
    (draw-line (type: (num num num num -> void)))
    (set-brush (type: (wxbrush-object -> void)))
    (device-to-logical-x (type: (num -> num)))
    (draw-point (type: (num num -> void)))
    (set-font (type: (wxfont-object -> void)))
    (ok? (type: (-> bool)))
    (set-optimization (type: (bool -> void)))
    (device-to-logical-y (type: (num -> num)))
    (draw-rectangle
     (type: (num num num num -> void)))
    (set-logical-function (type: (num -> void)))
    (logical-to-device-x (type: (num -> num)))
    (draw-rounded-rectangle
     (type: (num num num num optional num -> void)))
    (draw-icon
     (type: (wxicon-object num num -> void)))
    (logical-to-device-y (type: (num -> num)))
    (draw-spline
     (type: (num num num num num num -> void)))
    (set-text-background
     (type: (wxcolour-object -> void)))
    (try-colour
     (type: (wxcolour-object wxcolour-object -> void)))
    (start-doc (type: (str -> bool)))
    (draw-text
     (type: (str num num optional bool -> void)))
    (set-map-mode (type: (num -> void)))
    (start-page (type: (-> void)))
    (draw-arc
     (type: (num num num num num num -> void)))
    (set-text-foreground
     (type: (wxcolour-object -> void)))
    (blit (type: (num num
		  num
		  num
		  wxcanvasdc-object
		  num
		  num
		  optional
		  num
		  ->
		  bool)))
    (set-colour-map
     (type: (wxcolourmap-object -> void)))
    (end-page (type: (-> void)))
    (get-pen (type: (-> wxpen-object)))
    (draw-ellipse (type: (num num num num -> void)))
    (max-x (type: (-> num)))
    (set-user-scale (type: (num num -> void)))
    (max-y (type: (-> num)))
    (draw-lines
     (type: (wxpoint-object
	     optional
	     num
	     optional
	     num
	     ->
	     void)))
    (min-x (type: (-> num)))
    (get-background (type: (-> wxbrush-object)))
    (min-y (type: (-> num)))
    (end-doc (type: (-> void)))
    (get-brush (type: (-> wxbrush-object)))
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
    (get-font (type: (-> wxfont-object))))))

 (define wx:keymap%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (break-sequence (type: (-> void)))
    (map-function (type: (str str -> void)))
    (set-break-sequence-callback
     (type: ((-> void) -> void)))
    (implies-shift (type: (str -> void)))
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
     (type:         
	(union 
	 (str wxobject-object
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
    (get-double-click-interval (type: (-> num)))
    (chain-to-keymap
     (type: (wxkeymap-object bool -> void)))
    (set-double-click-interval (type: (num -> void)))
    (remove-chained-keymap
     (type: (wxkeymap-object -> void)))
    (remove-grab-key-function (type: (-> void)))
    (handle-key-event
     (type: (wxobject-object wxkeyevent-object -> bool)))
    (handle-mouse-event
     (type: (wxobject-object wxmouseevent-object -> bool)))
    (remove-grab-mouse-function (type: (-> void))))))

 (define wx:menu%
  (wxr:class* this
   ((super wx:object%))
   ((init1 (type: str))
    (init2 (type: (wxmenu-object wxevent-object -> void))))
   (public
    (enable (type: (num bool -> void)))
    (set-title (type: (str -> void)))
    (get-label (type: (num -> str)))
    (find-item (type: (str -> num)))
    (delete (type: (num -> void)))
    (get-help-string (type: (num -> str)))
    (append
     (type:
	(union
	 (num str wxmenu-object optional str -> void)
	 (num str optional str optional bool -> void))))
    (delete-by-position (type: (num -> void)))
    (set-help-string (type: (num str -> void)))
    (append-separator (type: (-> void)))
    (set-label (type: (num str -> void)))
    (check (type: (num bool -> void)))
    (checked? (type: (num -> bool)))
    (get-title (type: (-> str))))))

 (define wx:brush-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (find-or-create-brush
     (type: (union
	     (wxcolour-object num -> wxbrush-object)
	     (str num -> wxbrush-object))))
    (remove-brush (type: (wxbrush-object -> void))))))

 (define wx:clipboard-client%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (being-replaced (type: (-> void)))
    (get-data (type: (str -> str)))
    (add-type (type: (str -> void)))
    (get-types (type: (-> str))))))

 (define wx:event%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-event-object
     (type: (union 
	     (wxobject-object -> void)
	     (-> wxobject-object))))
    (get-event-type
     (type: (union (num -> void) (-> num))))
    (get-event-class
     (type: (union (num -> void) (-> num)))))))

 (define wx:printer-dc%
  (wxr:class* this
   ((super wx:dc%))
   (init1-arg-str
    init2-arg-str
    init3-arg-str
    (init4 (type: bool)))
   (public)))

 (define wx:window%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (on-size (type: (num num -> void)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (enable (type: (bool -> void)))
    (centre (type: (optional num -> void)))
    (on-set-focus (type: (-> void)))
    (show (type: (bool -> void)))
    (center (type: (num -> void)))
    (get-size (type: ((box num) (box num) -> void)))
    (set-focus (type: (-> void)))
    (set-size
     (type: (num num num num optional num -> void)))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (get-client-size
     (type: ((box num) (box num) -> void)))
    (refresh (type: (-> void)))
    (client-to-screen
     (type: ((box num) (box num) -> void)))
    (fit (type: (-> void)))
    (screen-to-client
     (type: ((box num) (box num) -> void)))
    (get-parent
     (type: (-> (union null wxwindow-object))))
    (get-position
     (type: ((box num) (box num) -> void)))
    (move (type: (num num -> void)))
    (get-grand-parent
     (type: (-> (union null wxwindow-object))))
    (on-kill-focus (type: (-> void)))
    (get-label (type: (-> str)))
    (get-char-height (type: (-> num)))
    (get-char-width (type: (-> num)))
    (set-cursor
     (type: (wxcursor-object -> (union null wxcursor-object))))
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
	     void))))))

 (define wx:canvas%
  (wxr:class* this
   ((super wx:window%))
   (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: str)))
   (public
    (destroy-clipping-region (type: (-> void)))
    (warp-pointer (type: (num num -> void)))
    (scroll (type: (num num -> void)))
    (enable-scrolling (type: (bool bool -> void)))
    (on-size (type: (num num -> void)))
    (set-clipping-region
     (type: (num num num num -> void)))
    (set-pen (type: (wxpen-object -> void)))
    (on-paint (type: (-> void)))
    (clear (type: (-> void)))
    (get-clipping-region
     (type: ((box num) (box num) (box num) (box num) -> void)))
    (on-event (type: (wxmouseevent-object -> void)))
    (begin-drawing (type: (-> void)))
    (set-background (type: (wxbrush-object -> void)))
    (end-drawing (type: (-> void)))
    (get-scroll-units-per-page
     (type: ((box num) (box num) -> void)))
    (popup-menu
     (type: (wxmenu-object num num -> bool)))
    (draw-line (type: (num num num num -> void)))
    (set-brush (type: (wxbrush-object -> void)))
    (draw-point (type: (num num -> void)))
    (set-font (type: (wxfont-object -> void)))
    (get-virtual-size
     (type: ((box num) (box num) -> void)))
    (draw-rectangle
     (type: (num num num num -> void)))
    (set-logical-function (type: (num -> void)))
    (is-retained? (type: (-> bool)))
    (draw-rounded-rectangle
     (type: (num num num num optional num -> void)))
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
    (set-text-background
     (type: (wxcolour-object -> void)))
    (view-start
     (type: ((box num) (box num) -> void)))
    (draw-text
     (type: (str num num optional bool -> void)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (draw-arc
     (type: (num num num num num num -> void)))
    (set-text-foreground
     (type: (wxcolour-object -> void)))
    (on-kill-focus (type: (-> void)))
    (draw-ellipse (type: (num num num num -> void)))
    (on-set-focus (type: (-> void)))
    (draw-lines
     (type: (wxpoint-object
	     optional
	     num
	     optional
	     num
	     ->
	     void)))
    (on-char
     (type: (union
	     (wxkeyevent-object -> void)
	     (wxkeyevent-object -> void))))
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
    (allow-double-click (type: (bool -> void)))
    (get-dc (type: (-> wxcanvasdc-object))))))

 (define wx:snip-class-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (find (type: (str -> (union null wxsnipclass-object))))
    (add (type: (wxsnipclass-object -> void)))
    (nth (type: (num -> (union null wxsnipclass-object))))
    (find-position
     (type: (wxsnipclass-object -> num)))
    (number (type: (-> num))))))

 (define wx:item%
  (wxr:class* this
   ((super wx:window%))
   ()
   (public
    (get-label (type: (-> str)))
    (set-label-colour
     (type: (union
	     (wxcolour-object -> void)
	     (wxcolour-object -> void))))
    (get-button-colour (type: (-> wxcolour-object)))
    (get-char-height (type: (-> num)))
    (set-button-colour
     (type: (union
	     (wxcolour-object -> void)
	     (wxcolour-object -> void))))
    (command (type: (wxcommandevent-object -> void)))
    (get-label-colour (type: (-> wxcolour-object)))
    (get-char-width (type: (-> num)))
    (set-label (type: (str -> void)))
    (get-background-colour
     (type: (-> wxcolour-object)))
    (set-background-colour
     (type: (union 
	     (wxcolour-object -> void)
	     (wxcolour-object -> void)))))))

 (define wx:canvas-dc%
  (wxr:class* this
   ((super wx:dc%))
   ()
   (public
    (get-pixel
     (type: (num num (union null wxcolour-object) -> bool)))
    (begin-set-pixel (type: (-> void)))
    (end-set-pixel (type: (-> void)))
    (set-pixel
     (type: (num num (union null wxcolour-object) -> void))))))

 (define wx:media-stream-out-base%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (write (type: ((listof char) -> void)))
    (tell (type: (-> num)))
    (seek (type: (num -> void)))
    (bad? (type: (-> bool))))))

 (define wx:panel%
  (wxr:class* this
   ((super wx:canvas%))
   (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: str)))
   (public
    (set-button-font (type: (wxfont-object -> void)))
    (set-label-colour
     (type: (wxcolour-object -> void)))
    (get-button-font (type: (-> wxfont-object)))
    (on-paint
     (union (type: (-> void)) (type: (-> void))))
    (set-button-colour
     (type: (wxcolour-object -> void)))
    (set-label-font (type: (wxfont-object -> void)))
    (on-event
     (union (type: (wxmouseevent-object -> void))
      (type: (wxmouseevent-object -> void))))
    (get-label-font (type: (-> wxfont-object)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (get-default-item (type: (-> wxbutton-object)))
    (advance-cursor
     (type: (wxwindow-object -> void)))
    (fit (type: (-> void)))
    (get-item-cursor
     (type: ((box num) (box num) -> void)))
    (get-child (type: (num -> wxobject-object)))
    (set-item-cursor (type: (num num -> void)))
    (on-kill-focus (type: (-> void)))
    (on-default-action
     (type: (wxitem-object -> void)))
    (tab (union (type: (-> void)) (type: (num -> void))))
    (on-size (type: (num num -> void)))
    (set-label-position (type: (num -> void)))
    (get-button-colour (type: (-> wxcolour-object)))
    (get-horizontal-spacing (type: (-> num)))
    (get-label-colour (type: (-> wxcolour-object)))
    (get-vertical-spacing (type: (-> num)))
    (get-background-colour
     (type: (-> wxcolour-object)))
    (set-horizontal-spacing (type: (num -> void)))
    (on-set-focus (type: (-> void)))
    (set-vertical-spacing (type: (num -> void)))
    (new-line
     (union (type: (-> void)) (type: (num -> void))))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (on-char
     (union (type: (wxkeyevent-object -> void))
      (type: (wxkeyevent-object -> void))))
    (get-panel-dc (type: (-> wxdc-object)))
    (set-background-colour
     (type: (wxcolour-object -> void))))))

 (define wx:style-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (basic-style (type: (-> wxstyle-object)))
    (replace-named-style
     (type: (str (union null wxstyle-object)
	     ->
	     wxstyle-object)))
    (find-or-create-join-style
     (type: ((union null wxstyle-object)
	     wxstyle-object
	     ->
	     wxstyle-object)))
    (find-or-create-style
     (type: ((union null wxstyle-object)
	     wxstyledelta-object
	     ->
	     wxstyle-object)))
    (index-to-style
     (type: (num -> (union null wxstyle-object))))
    (clear (type: (-> void)))
    (copy (type: (wxstylelist-object -> void)))
    (adjust-usage (type: (bool -> void)))
    (find-named-style
     (type: (str -> wxstyle-object)))
    (style-to-index (type: (wxstyle-object -> num)))
    (convert
     (type: (wxstyle-object -> wxstyle-object)))
    (is-used? (type: (-> bool)))
    (new-named-style
     (type: (str (union null wxstyle-object)
	     ->
	     wxstyle-object)))
    (number (type: (-> num))))))

 (define wx:snip%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-text (type: (num num optional bool -> str)))
    (size-cache-invalid (type: (-> void)))
    (on-event
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxmouseevent-object
	     ->
	     void)))
    (get-admin (type: (-> wxsnipadmin-object)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (do-font (type: (num optional bool -> void)))
    (get-snipclass
     (union (type: ((union null wxsnipclass-object) -> void))
      (type: (-> (union null wxsnipclass-object)))))
    (set-admin (type: (wxsnipadmin-object -> void)))
    (get-count (type: (-> num)))
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
    (set-count (type: (num -> void)))
    (match? (type: (wxsnip-object -> bool)))
    (next (type: (-> (union null wxsnip-object))))
    (set-flags (type: (num -> void)))
    (is-owned? (type: (-> bool)))
    (copy (type: (-> wxsnip-object)))
    (get-style (type: (-> wxstyle-object)))
    (release-from-owner (type: (-> bool)))
    (resize (type: (num num -> bool)))
    (partial-offset
     (type: (wxdc-object num num num -> num)))
    (merge-with
     (type: (wxsnip-object -> wxsnip-object)))
    (previous
     (type: (-> (union null wxsnip-object))))
    (write (type: (wxmediastreamout-object -> void)))
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
    (on-char
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxkeyevent-object
	     ->
	     void)))
    (own-caret (type: (bool -> void)))
    (get-flags (type: (-> num))))))

 (define wx:media-stream-in-base%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (read (type: ((listof char) -> num)))
    (tell (type: (-> num)))
    (seek (type: (num -> void)))
    (skip (type: (num -> void)))
    (bad? (type: (-> bool))))))

 (define wx:slider%
  (wxr:class* this
   ((super wx:item%))
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
    (get-value (type: (-> num)))
    (set-value (type: (num -> void))))))

 (define wx:list-box%
  (wxr:class* this
   ((super wx:item%))
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
    (set-string-selection (type: (str -> void)))
    (clear (type: (-> void)))
    (append
     (union (type: (str -> void))
      (type: (str str -> void))))
    (deselect (type: (num -> void)))
    (set-first-item
     (union (type: (num -> void))
      (type: (str -> void))))
    (get-selection (type: (-> num)))
    (get-string (type: (num -> str)))
    (selected? (type: (num -> bool)))
    (get-string-selection (type: (-> str)))
    (get-client-data (type: (num -> str)))
    (delete (type: (num -> void)))
    (set-selection
     (type: (num optional bool -> void)))
    (get-selections (type: ((box (box num)) -> num)))
    (number (type: (-> num)))
    (set (type: ((listof str) -> void)))
    (find-string (type: (str -> num))))))

 (define wx:media-stream-out-string-base%
  (wxr:class* this
   ((super wx:media-stream-out-base%))
   ()
   (public (get-string (type: (-> str))))))

 (define wx:group-box%
  (wxr:class* this
   ((super wx:item%))
   (init1-arg-wxpanel-object
    init2-arg-str
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: num))
    (init8 (type: str)))
   (public)))

 (define wx:choice%
  (wxr:class* this
   ((super wx:item%))
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
    (set-string-selection (type: (str -> void)))
    (append (type: (str -> void)))
    (get-selection (type: (-> num)))
    (get-string (type: (num -> str)))
    (clear (type: (-> void)))
    (get-string-selection (type: (-> str)))
    (set-columns (type: (optional num -> void)))
    (set-selection (type: (num -> void)))
    (get-columns (type: (-> num)))
    (find-string (type: (str -> num))))))

 (define wx:clipboard%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (set-clipboard-client
     (type: (wxclipboardclient-object num -> void)))
    (get-clipboard-data (type: (str num -> str)))
    (set-clipboard-string (type: (str num -> void)))
    (get-clipboard-client
     (type: (-> (union null wxclipboardclient-object))))
    (get-clipboard-string (type: (num -> str))))))

 (define wx:memory-dc%
  (wxr:class* this
   ((super wx:canvas-dc%))
   (init1-arg-wxcanvasdc-object)
   (public
    (select-object
     (type: ((union null wxbitmap-object) -> void))))))

 (define wx:media-snip%
  (wxr:class* this
   ((super wx:snip%))
   ((init1 (type: (union null wxmediabuffer-object)))
    (init2 (type: bool))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: num))
    (init8 (type: num))
    (init9 (type: num))
    (init10 (type: num)))
   (public
    (get-text (type: (num num optional bool -> str)))
    (size-cache-invalid (type: (-> void)))
    (get-max-width (type: (-> num)))
    (on-event
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxmouseevent-object
	     ->
	     void)))
    (get-min-width (type: (-> num)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (set-max-width (type: (num -> void)))
    (set-margin (type: (num num num num -> void)))
    (set-admin (type: (wxsnipadmin-object -> void)))
    (get-margin
     (type: ((box num) (box num) (box num) (box num) -> void)))
    (border-visible? (type: (-> bool)))
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
    (set-inset (type: (num num num num -> void)))
    (match? (type: (wxsnip-object -> bool)))
    (get-min-height (type: (-> num)))
    (get-this-media
     (type: (-> (union null wxmediabuffer-object))))
    (do-font (type: (num optional bool -> void)))
    (merge-with
     (type: (wxsnip-object -> wxsnip-object)))
    (set-max-height (type: (num -> void)))
    (set-min-width (type: (num -> void)))
    (copy (type: (-> wxsnip-object)))
    (set-min-height (type: (num -> void)))
    (resize (type: (num num -> bool)))
    (get-max-height (type: (-> num)))
    (partial-offset
     (type: (wxdc-object num num num -> num)))
    (show-border (type: (bool -> void)))
    (get-inset
     (type: ((box num) (box num) (box num) (box num) -> void)))
    (write (type: (wxmediastreamout-object -> void)))
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
    (on-char
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxkeyevent-object
	     ->
	     void)))
    (own-caret (type: (bool -> void))))))

 (define wx:message%
  (wxr:class* this
   ((super wx:item%))
   (init1-arg-wxpanel-object
    init2-arg-str
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: str)))
   (public)))

 (define wx:cursor%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-str-or-init1-arg-num
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num)))
   (public (ok? (type: (-> bool))))))

 (define wx:button%
  (wxr:class* this
   ((super wx:item%))
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
    (set-default (type: (-> void)))
    (set-label
     (union (type: (wxbitmap-object -> void))
      (type: (str -> void)))))))

 (define wx:radio-box%
  (wxr:class* this
   ((super wx:item%))
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
    (set-string-selection (type: (str -> void)))
    (get-selection (type: (-> num)))
    (get-string (type: (num -> str)))
    (get-string-selection (type: (-> str)))
    (set-selection (type: (num -> void)))
    (number (type: (-> num)))
    (find-string (type: (str -> num))))))

 (define wx:media-stream-in-string-base%
  (wxr:class* this
   ((super wx:media-stream-in-base%))
   (init1-arg-str)
   (public)))

 (define wx:timer%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (notify (type: (-> void)))
    (start (type: (num optional bool -> bool)))
    (interval (type: (-> num)))
    (stop (type: (-> void))))))

 (define wx:text-window%
  (wxr:class* this
   ((super wx:window%))
   (init1-arg-wxframe-object-or-init1-arg-wxpanel-object
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: str)))
   (public
    (get-line-length (type: (num -> num)))
    (replace (type: (num num str -> void)))
    (get-number-of-lines (type: (-> num)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (position-to-x-y
     (type: (num (box num) (box num) -> void)))
    (x-y-to-position (type: (num num -> num)))
    (set-insertion-point (type: (num -> void)))
    (modified? (type: (-> bool)))
    (popup-menu
     (type: (wxmenu-object num num -> bool)))
    (set-insertion-point-end (type: (-> void)))
    (cut (type: (-> void)))
    (show-position (type: (num -> void)))
    (write-text (type: (str -> void)))
    (load-file (type: (str -> bool)))
    (set-selection (type: (num num -> void)))
    (set-font (type: (wxfont-object -> void)))
    (on-size (type: (num num -> void)))
    (remove (type: (num num -> void)))
    (copy (type: (-> void)))
    (paste (type: (-> void)))
    (clear (type: (-> void)))
    (on-kill-focus (type: (-> void)))
    (discard-edits (type: (-> void)))
    (on-set-focus (type: (-> void)))
    (get-contents (type: (-> str)))
    (get-insertion-point (type: (-> num)))
    (save-file (type: (str -> bool)))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (get-last-position (type: (-> num)))
    (on-char (type: (wxkeyevent-object -> void))))))

 (define wx:dialog-box%
  (wxr:class* this
   ((super wx:panel%))
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
    (on-size (type: (num num -> void)))
    (on-paint (type: (-> void)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (on-default-action
     (type: (wxitem-object -> void)))
    (on-event (type: (wxmouseevent-object -> void)))
    (on-activate (type: (bool -> void)))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (on-char (type: (wxkeyevent-object -> void)))
    (on-close (type: (-> bool)))
    (on-kill-focus (type: (-> void))))))

 (define wx:brush%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-wxcolour-object-or-init1-arg-str
    init2-arg-num-or-init2-arg-num)
   (public
    (get-style (type: (-> num)))
    (get-colour (type: (-> wxcolour-object)))
    (set-style (type: (num -> void)))
    (set-colour
     (union (type: (wxcolour-object -> void))
      (union (type: (str -> void))
       (type: (num num num -> void)))))
    (set-stipple (type: (wxbitmap-object -> void)))
    (get-stipple (type: (-> wxbitmap-object))))))

 (define wx:snip-class%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-version
     (union (type: (num -> void)) (type: (-> num))))
    (read-done (type: (-> void)))
    (read (type: (wxmediastreamin-object
		  ->
		  (union null wxsnip-object))))
    (get-classname
     (union (type: (str -> void)) (type: (-> str))))
    (write-header
     (type: (wxmediastreamout-object -> bool)))
    (read-header
     (type: (wxmediastreamin-object -> bool)))
    (write-done (type: (-> void))))))

 (define wx:media-buffer%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (set-max-undo-history (type: (num -> void)))
    (get-max-undo-history (type: (-> num)))
    (append-edit-items
     (type: (wxmenu-object optional num -> num)))
    (append-font-items
     (type: (wxmenu-object optional num -> num)))
    (refresh (type: (num num num num bool -> void)))
    (insert (type: (wxsnip-object -> void)))
    (set-keymap
     (type: (optional (union null wxkeymap-object) -> void)))
    (get-buffer-type
     (union (type: (num -> void)) (type: (-> num))))
    (own-caret (type: (bool -> void)))
    (get-keymap (type: (-> wxkeymap-object)))
    (copy (type: (optional bool optional num -> void)))
    (copy-self (type: (-> wxmediabuffer-object)))
    (size-cache-invalid (type: (-> void)))
    (add-buffer-functions
     (type: (wxkeymap-object -> void)))
    (kill (type: (optional num -> void)))
    (get-extent
     (type: ((union null (box num))
	     (union null (box num))
	     ->
	     void)))
    (on-local-event
     (type: (wxmouseevent-object -> void)))
    (get-style-list (type: (-> wxstylelist-object)))
    (read-from-file
     (union (type: (wxmediastreamin-object -> bool))
      (type: (wxmediastreamin-object -> bool))))
    (on-local-char
     (type: (wxkeyevent-object -> void)))
    (write-to-file
     (union (type: (wxmediastreamout-object -> bool))
      (type: (wxmediastreamout-object -> bool))))
    (on-default-event
     (type: (wxmouseevent-object -> void)))
    (set-style-list
     (type: (wxstylelist-object -> void)))
    (redo (type: (-> void)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (on-default-char
     (type: (wxkeyevent-object -> void)))
    (begin-write-header-footer-to-file
     (type: (wxmediastreamout-object str (box num) -> bool)))
    (do-font (type: (num optional bool -> void)))
    (resized (type: (wxsnip-object bool -> void)))
    (get-min-width (type: (-> num)))
    (on-focus (type: (bool -> void)))
    (get-filename
     (type: (optional (union null (box bool)) -> str)))
    (print (type: (optional str optional bool -> void)))
    (set-max-width (type: (num -> void)))
    (on-change (type: (-> void)))
    (end-write-header-footer-to-file
     (type: (wxmediastreamout-object num -> bool)))
    (set-min-width (type: (num -> void)))
    (get-snip-data
     (type: (wxsnip-object
	     ->
	     (union null wxbufferdata-object))))
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
    (set-snip-data
     (type: (wxsnip-object
	     (union null wxbufferdata-object)
	     ->
	     void)))
    (get-inactive-caret-threshold (type: (-> num)))
    (get-min-height (type: (-> num)))
    (get-view-size
     (type: ((union null (box num))
	     (union null (box num))
	     ->
	     void)))
    (set-modified (type: (bool -> void)))
    (set-max-height (type: (num -> void)))
    (set-caret-owner
     (type: ((union null wxsnip-object) -> void)))
    (set-filename
     (type: (str optional bool -> void)))
    (set-inactive-caret-threshold
     (type: (num -> void)))
    (set-min-height (type: (num -> void)))
    (read-header-from-file
     (type: (wxmediastreamin-object str -> bool)))
    (style-has-changed
     (type: ((union null wxstyle-object) -> void)))
    (get-space (type: (-> num)))
    (release-snip (type: (wxsnip-object -> bool)))
    (read-footer-from-file
     (type: (wxmediastreamin-object str -> bool)))
    (begin-edit-sequence
     (type: (optional bool -> void)))
    (write-headers-to-file
     (type: (wxmediastreamout-object -> bool)))
    (end-edit-sequence (type: (-> void)))
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
    (modified? (type: (-> bool)))
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
    (scroll-line-location (type: (num num -> num)))
    (scroll-to
     (type: (wxsnip-object num num num num bool -> bool)))
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
    (num-scroll-lines (type: (num -> num)))
    (on-new-image-snip
     (type: (str num bool bool -> wximagesnip-object)))
    (find-scroll-line (type: (num num -> num)))
    (needs-update
     (type: (wxsnip-object num num num num -> void)))
    (cut (type: (optional bool optional num -> void)))
    (on-new-box (type: (num -> wxsnip-object)))
    (print-to-dc (type: (wxdc-object -> void)))
    (insert-box (type: (optional num -> void)))
    (on-char (type: (wxkeyevent-object -> void)))
    (on-save-file (type: (str num -> bool)))
    (paste (type: (optional num -> void)))
    (get-admin
     (type: (-> (union null wxmediaadmin-object))))
    (after-save-file (type: (bool -> void)))
    (on-load-file (type: (str num -> bool)))
    (set-admin
     (type: ((union null wxmediaadmin-object) -> void)))
    (get-dc (type: (-> wxdc-object)))
    (get-descent (type: (-> num)))
    (after-load-file (type: (bool -> void)))
    (global-to-local
     (type: ((box num) (box num) -> void)))
    (get-focus-snip
     (type: (-> (union null wxsnip-object))))
    (get-file (type: (str -> str)))
    (undo (type: (-> void)))
    (local-to-global
       (type: ((box num) (box num) -> void)))
       (clear (type: (-> void)))
       (put-file (type: (str str -> str)))
       (select-all (type: (-> void)))
       (on-event (type: (wxmouseevent-object -> void)))
       (get-max-width (type: (-> num)))
       (get-flattened-text (type: (-> str)))
       (clear-undos (type: (-> void)))
       (lock (type: (bool -> void))))))

 (define wx:text%
  (wxr:class* this
   ((super wx:item%))
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
    (cut (type: (-> void)))
    (copy (type: (-> void)))
    (get-value (type: (-> str)))
    (paste (type: (-> void)))
    (set-value (type: (str -> void)))
    (set-editable (type: (bool -> void)))
    (on-char (type: (wxkeyevent-object -> void))))))

 (define wx:int-point%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-num init2-arg-num)
   (public
    (get-y (union (type: (num -> void)) (type: (-> num))))
    (get-x (union (type: (num -> void)) (type: (-> num)))))))

 (define wx:media-edit%
  (wxr:class* this
   ((super wx:media-buffer%))
   ((init1 (type: num))
    (init2 (type: (listof num))))
   (public
    (on-focus (type: (bool -> void)))
    (get-wordbreak-map
     (type: (-> (union null wxmediawordbreakmap-object))))
    (last-paragraph (type: (-> num)))
    (get-start-position (type: (-> num)))
    (hide-caret (type: (bool -> void)))
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
	     -> (listof num))))
    (get-end-position (type: (-> num)))
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
    (refresh (type: (num num num num bool -> void)))
    (get-snip-position
     (type: (wxsnip-object -> num)))
    (move-position
     (type: (num optional bool optional num -> void)))
    (insert
     (union (type: (wxsnip-object -> void))
      (union (type: (str num optional num optional bool -> void))
       (union (type: (str -> void))
	(union (type: (num str
		       num
		       optional
		       num
		       optional
		       bool
		       ->
		       void))
	 (union (type: (num str -> void))
	  (union (type: (wxsnip-object
			 num
			 optional
			 num
			 ->
			 void))
	   (union (type: (char -> void))
	    (type: (char num
		    optional
		    num
		    ->
		    void))))))))))
    (erase (type: (-> void)))
    (on-new-text-snip (type: (-> wxtextsnip-object)))
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
    (cut (union (type: (optional bool optional num -> void))
	  (type: (bool num num optional num -> void))))
    (copy (union (type: (optional bool optional num -> void))
	   (type: (bool num num optional num -> void))))
    (get-character (type: (num -> char)))
    (get-visible-line-range
     (type: ((union null (box num))
	     (union null (box num))
	     ->
	     void)))
    (paste (union (type: (optional num -> void))
	    (type: (num num optional num -> void))))
    (kill (union (type: (optional num -> void))
	   (type: (num num num -> void))))
    (on-new-tab-snip (type: (-> wxtabsnip-object)))
    (insert-file (type: (str optional num -> bool)))
    (set-anchor (type: (bool -> void)))
    (do-copy (type: (num num num bool -> void)))
    (load-file
     (type: (optional str optional num -> bool)))
    (set-autowrap-bitmap
     (type: ((union null wxbitmap-object)
	     ->
	     (union null wxbitmap-object))))
    (read-from-file
     (union (type: (wxmediastreamin-object -> bool))
      (type: (wxmediastreamin-object num -> bool))))
    (get-anchor (type: (-> bool)))
    (write-footers-to-file
     (type: (wxmediastreamout-object -> bool)))
    (set-wordbreak-func
     (type: ((wxmediaedit-object
	      (box num)
	      (box num)
	      num
	      ->
	      void)
	     ->
	     void)))
    (write-to-file
     (union (type: (wxmediastreamout-object -> bool))
      (type: (wxmediastreamout-object
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
    (on-default-event
     (type: (wxmouseevent-object -> void)))
    (get-file-format (type: (-> num)))
    (flash-off (type: (-> void)))
    (on-default-char
     (type: (wxkeyevent-object -> void)))
    (save-file
     (type: (optional str optional num -> bool)))
    (set-file-format (type: (num -> void)))
    (paste-next (type: (-> void)))
    (read-footer-from-file
     (type: (wxmediastreamin-object str -> bool)))
    (delete
     (union (type: (num optional num optional bool -> void))
      (type: (-> void))))
    (set-clickback
     (type: (num num
	     (wxmediaedit-object num num -> void)
	     optional
	     (union null wxstyledelta-object)
	     optional
	     bool
	     ->
	     void)))
    (get-overwrite-mode (type: (-> bool)))
    (do-paste (type: (num num -> void)))
    (on-change (type: (-> void)))
    (write-headers-to-file
     (type: (wxmediastreamout-object -> bool)))
    (remove-clickback (type: (num num -> void)))
    (set-overwrite-mode (type: (bool -> void)))
    (get-snip-data
     (type: (wxsnip-object
	     ->
	     (union null wxbufferdata-object))))
    (change-style
     (union (type: ((union null wxstyledelta-object) -> void))
      (union (type: ((union null wxstyledelta-object)
		     num
		     optional
		     num
		     ->
		     void))
       (type: ((union null wxstyle-object)
	       optional
	       num
	       optional
	       num
	       ->
	       void)))))
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
    (get-tabs
     (type: (optional
	     (union null (box num))
	     optional
	     (union null (box num))
	     optional
	     (union null (box bool))
	     -> (listof num))))
    (split-snip (type: (num -> void)))
    (set-modified (type: (bool -> void)))
    (set-caret-owner
     (type: ((union null wxsnip-object) -> void)))
    (set-tabs
     (type: ((listof num) optional num optional bool -> void)))
    (find-line
     (type: (num optional (union null (box bool)) -> num)))
    (get-visible-position-range
     (type: ((union null (box num))
	     (union null (box num))
	     ->
	     void)))
    (on-local-char
     (type: (wxkeyevent-object -> void)))
    (add-editor-functions
     (type: (wxkeymap-object -> void)))
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
    (read-header-from-file
     (type: (wxmediastreamin-object str -> bool)))
    (release-snip (type: (wxsnip-object -> bool)))
    (on-insert (type: (num num -> bool)))
    (get-between-threshold (type: (-> num)))
    (paragraph-start-position
     (type: (num optional bool -> num)))
    (after-insert (type: (num num -> void)))
    (set-between-threshold (type: (num -> void)))
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
    (on-char (type: (wxkeyevent-object -> void)))
    (on-delete (type: (num num -> bool)))
    (position-line
     (type: (num optional bool -> num)))
    (get-snip-position-and-location
     (type: (wxsnip-object
	     (box num)
	     (box num)
	     (box num)
	     ->
	     void)))
    (after-delete (type: (num num -> void)))
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
    (resized (type: (wxsnip-object bool -> void)))
    (scroll-to
     (type: (wxsnip-object num num num num bool -> bool)))
    (on-change-style (type: (num num -> bool)))
    (line-location
     (type: (num optional bool -> num)))
    (after-set-size-constraint (type: (-> void)))
    (after-change-style (type: (num num -> void)))
    (line-start-position
     (type: (num optional bool -> num)))
    (on-new-image-snip
     (type: (str num bool bool -> wximagesnip-object)))
    (needs-update
     (type: (wxsnip-object num num num num -> void)))
    (own-caret (type: (bool -> void)))
    (on-edit-sequence (type: (-> void)))
    (line-end-position
     (type: (num optional bool -> num)))
    (on-new-box (type: (num -> wxsnip-object)))
    (set-cursor
     (type: ((union null wxcursor-object) -> void)))
    (after-edit-sequence (type: (-> void)))
    (line-length (type: (num -> num)))
    (on-save-file (type: (str num -> bool)))
    (size-cache-invalid (type: (-> void)))
    (after-set-position (type: (-> void)))
    (last-position (type: (-> num)))
    (after-save-file (type: (bool -> void)))
    (on-local-event
     (type: (wxmouseevent-object -> void)))
    (on-set-size-constraint (type: (-> bool)))
    (last-line (type: (-> num)))
    (on-load-file (type: (str num -> bool)))
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
    (get-region-data
     (type: (num num -> (union null wxbufferdata-object))))
    (position-paragraph
     (type: (num optional bool -> num)))
    (after-load-file (type: (bool -> void)))
    (set-snip-data
     (type: (wxsnip-object
	     (union null wxbufferdata-object)
	     ->
	     void)))
    (paragraph-end-position
     (type: (num optional bool -> num)))
    (get-file (type: (str -> str)))
    (set-region-data
     (type: (num num
	     (union null wxbufferdata-object)
	     ->
	     void)))
    (line-paragraph (type: (num -> num)))
    (put-file (type: (str str -> str)))
    (on-event (type: (wxmouseevent-object -> void)))
    (find-wordbreak
     (type: ((union null (box num))
	     (union null (box num))
	     num
	     ->
	     void)))
    (paragraph-start-line (type: (num -> num)))
    (get-flattened-text (type: (-> str)))
    (set-wordbreak-map
     (type: ((union null wxmediawordbreakmap-object) -> void)))
    (pargraph-end-line (type: (num -> num)))
    (get-position
     (type: ((union null (box num))
	     optional
	     (union null (box num))
	     ->
	     void)))
    (set-filename
     (type: (str optional bool -> void))))))

 (define wx:meta-file%
  (wxr:class* this
   ((super wx:object%))
   ((init1 (type: str)))
   (public
    (set-clipboard
     (type: (optional num optional num -> bool)))
    (ok? (type: (-> bool)))
    (play (type: (wxdc-object -> void))))))

 (define wx:pen%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-wxcolour-object-or-init1-arg-str
    init2-arg-num-or-init2-arg-num
    init3-arg-num-or-init3-arg-num)
   (public
    (get-cap (type: (-> num)))
    (set-width (type: (num -> void)))
    (set-cap (type: (num -> void)))
    (get-style (type: (-> num)))
    (get-join (type: (-> num)))
    (get-width (type: (-> num)))
    (get-colour (type: (-> wxcolour-object)))
    (set-style (type: (num -> void)))
    (set-join (type: (num -> void)))
    (set-colour
     (union (type: (wxcolour-object -> void))
      (union (type: (str -> void))
       (type: (num num num -> void)))))
    (set-stipple (type: (wxbitmap-object -> void)))
    (get-stipple (type: (-> wxbitmap-object))))))

 (define wx:point%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-num init2-arg-num)
   (public
    (get-y (union (type: (num -> void)) (type: (-> num))))
    (get-x (union (type: (num -> void)) (type: (-> num)))))))

 (define wx:multi-text%
  (wxr:class* this
   ((super wx:text%))
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

 (define wx:font%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-num-or-init1-arg-num
    init2-arg-num-or-init2-arg-str
    init3-arg-num-or-init3-arg-num
    init4-arg-num-or-init4-arg-num
    (init5 (type: bool))
    (init6 (type: bool)))
   (public
    (get-underlined (type: (-> bool)))
    (get-style (type: (-> num)))
    (get-point-size (type: (-> num)))
    (get-family (type: (-> num)))
    (get-weight (type: (-> num)))
    (get-font-id (type: (-> num))))))

 (define wx:pen-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (remove-pen (type: (wxpen-object -> void)))
    (find-or-create-pen
     (union (type: (wxcolour-object num num -> wxpen-object))
      (type: (str num num -> wxpen-object)))))))

 (define wx:media-stream-out%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-wxmediastreamoutbase-object)
   (public
    (<< (union (type: (str -> wxmediastreamout-object))
	 (union (type: (num -> wxmediastreamout-object))
	  (type: (num -> wxmediastreamout-object)))))
    (jump-to (type: (num -> void)))
    (tell (type: (-> num)))
    (ok? (type: (-> bool)))
    (put-fixed
     (type: (num -> wxmediastreamout-object)))
    (put (union (type: (num str -> wxmediastreamout-object))
	  (union (type: (str -> wxmediastreamout-object))
	   (union (type: (num -> wxmediastreamout-object))
	    (type: (num -> wxmediastreamout-object)))))))))

 (define wx:media-wordbreak-map%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (adjust-usage (type: (bool -> void)))
    (is-used? (type: (-> bool)))
    (set-map (type: (num num -> void)))
    (get-map (type: (num -> num))))))

 (define wx:style%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-name (type: (-> str)))
    (get-foreground (type: (-> wxcolour-object)))
    (get-size (type: (-> num)))
    (get-alignment (type: (-> num)))
    (get-base-style (type: (-> wxstyle-object)))
    (set-base-style (type: (wxstyle-object -> void)))
    (get-colour (type: (-> wxcolour-object)))
    (get-family (type: (-> num)))
    (get-delta (type: (wxstyledelta-object -> void)))
    (is-join? (type: (-> bool)))
    (get-style (type: (-> num)))
    (get-shift-style (type: (-> wxstyle-object)))
    (get-transparent-text-backing (type: (-> bool)))
    (set-shift-style
     (type: (wxstyle-object -> void)))
    (get-weight (type: (-> num)))
    (switch-to
     (type: (wxdc-object wxstyle-object -> void)))
    (get-face (type: (-> str)))
    (get-underlined (type: (-> bool)))
    (set-delta (type: (wxstyledelta-object -> void)))
    (get-font (type: (-> wxfont-object))))))

 (define wx:tool-bar%
  (wxr:class* this
   ((super wx:canvas%))
   (init1-arg-wxframe-object
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: num))
    (init8 (type: num)))
   (public
    (get-tool-enabled? (type: (num -> bool)))
    (find-tool-for-position
     (type: (num num -> (union null wxtoolbartool-object))))
    (get-tool-state? (type: (num -> bool)))
    (add-tool
     (type: (num wxbitmap-object
	     optional
	     (union null wxbitmap-object)
	     optional
	     bool
	     optional
	     num
	     optional
	     num
	     optional
	     (union null wxobject-object)
	     ->
	     wxtoolbartool-object)))
    (get-max-size
     (type: ((box num) (box num) -> void)))
    (set-margins (type: (num num -> void)))
    (enable-tool (type: (num bool -> void)))
    (get-tool-client-data
     (type: (num -> (union null wxobject-object))))
    (toggle-tool (type: (num bool -> void))))))

 (define wx:gauge%
  (wxr:class* this
   ((super wx:item%))
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
    (set-range (type: (num -> void)))
    (set-value (type: (num -> void))))))

 (define wx:snip-admin%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (set-cursor (type: (wxcursor-object -> void)))
    (release-snip (type: (wxsnip-object -> bool)))
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
    (get-view-size
     (type: ((union null (box num))
	     (union null (box num))
	     ->
	     void)))
    (resized (type: (wxsnip-object bool -> void)))
    (set-caret-owner (type: (wxsnip-object -> void)))
    (needs-update
     (type: (wxsnip-object num num num num -> void)))
    (get-media
     (type: (-> (union null wxmediabuffer-object))))
    (get-dc (type: (-> wxdc-object))))))

 (define wx:key-event%
  (wxr:class* this
   ((super wx:event%))
   (init1-arg-num)
   (public
    (get-alt-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-key-code
     (union (type: (num -> void)) (type: (-> num))))
    (get-meta-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (key-code (type: (-> num)))
    (get-control-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (position (type: ((box num) (box num) -> void)))
    (get-time-stamp
     (union (type: (num -> void)) (type: (-> num))))
    (get-shift-down
     (union (type: (bool -> void)) (type: (-> bool)))))))

 (define wx:colour-database%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (find-colour
     (type: (str -> (union null wxcolour-object))))
    (append (type: (str wxcolour-object -> void)))
    (find-name (type: (wxcolour-object -> str))))))

 (define wx:media-stream-in%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-wxmediastreaminbase-object)
   (public
    (get-fixed
     (type: ((box num) -> wxmediastreamin-object)))
    (>> (union (type: ((box num) -> wxmediastreamin-object))
	 (type: ((box num) -> wxmediastreamin-object))))
    (get-string
     (type: (optional (union null (box num)) -> str)))
    (jump-to (type: (num -> void)))
    (set-boundary (type: (num -> void)))
    (tell (type: (-> num)))
    (ok? (type: (-> bool)))
    (remove-boundary (type: (-> void)))
    (skip (type: (num -> void)))
    (get (union (type: ((box num) -> wxmediastreamin-object))
	  (type: ((box num) -> wxmediastreamin-object)))))))

 (define wx:colour-map%
  (wxr:class* this ((super wx:object%)) () (public)))

 (define wx:frame%
  (wxr:class* this
   ((super wx:window%))
   (|init1-arg-(union null wxframe-object)|
    init2-arg-str
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: num))
    (init8 (type: str)))
   (public
    (load-accelerators (type: (str -> void)))
    (on-size (type: (num num -> void)))
    (create-status-line
     (type: (optional num optional str -> void)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (on-activate (type: (bool -> void)))
    (on-close (type: (-> bool)))
    (get-title (type: (-> str)))
    (on-menu-command (type: (num -> void)))
    (set-title (type: (str -> void)))
    (on-menu-select (type: (num -> void)))
    (set-icon (type: (wxicon-object -> void)))
    (command (type: (num -> void)))
    (set-menu-bar (type: (wxmenubar-object -> void)))
    (get-menu-bar
     (union (type: (-> (union null wxmenubar-object)))
      (type: (-> (union null wxmenubar-object)))))
    (iconize (type: (bool -> void)))
    (set-tool-bar
     (type: ((union null wxwindow-object) -> void)))
    (get-tool-bar
     (type: (-> (union null wxwindow-object))))
    (on-kill-focus (type: (-> void)))
    (set-status-text (type: (str -> void)))
    (on-set-focus (type: (-> void)))
    (iconized? (type: (-> bool)))
    (status-line-exists? (type: (-> bool)))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (maximize (type: (bool -> void))))))

 (define wx:image-snip%
  (wxr:class* this
   ((super wx:snip%))
   ((init1 (type: str))
    (init2 (type: num))
    (init3 (type: bool))
    (init4 (type: bool)))
   (public
    (get-text (type: (num num optional bool -> str)))
    (size-cache-invalid (type: (-> void)))
    (on-event
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxmouseevent-object
	     ->
	     void)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (do-font (type: (num optional bool -> void)))
    (set-admin (type: (wxsnipadmin-object -> void)))
    (get-filename
     (type: ((union null (box bool)) -> str)))
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
    (match? (type: (wxsnip-object -> bool)))
    (load-file
     (type: (str num optional bool optional bool -> void)))
    (copy (type: (-> wxsnip-object)))
    (get-filetype (type: (-> num)))
    (resize (type: (num num -> bool)))
    (set-bitmap (type: (wxbitmap-object -> void)))
    (partial-offset
     (type: (wxdc-object num num num -> num)))
    (set-offset (type: (num num -> void)))
    (merge-with
     (type: (wxsnip-object -> wxsnip-object)))
    (write (type: (wxmediastreamout-object -> void)))
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
    (on-char
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxkeyevent-object
	     ->
	     void)))
    (own-caret (type: (bool -> void))))))

 (define wx:colour%
  (wxr:class* this
   ((super wx:object%))
   (init1-arg-num-or-init1-arg-str
    init2-arg-num
    init3-arg-num)
   (public
    (red (type: (-> num)))
    (green (type: (-> num)))
    (blue (type: (-> num)))
    (ok? (type: (-> bool)))
    (get (type: ((box num) (box num) (box num) -> void)))
    (= (type: (wxcolour-object -> wxcolour-object)))
    (set (type: (num num num -> void))))))

 (define wx:style-delta%
  (wxr:class* this
   ((super wx:object%))
   ((init1 (type: num)) (init2 (type: num)))
   (public
    (get-transparent-text-backing-on
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-foreground-add
     (type: (-> wxaddcolour-object)))
    (set-delta-foreground
     (union (type: (str -> wxstyledelta-object))
      (type: (wxcolour-object -> wxstyledelta-object))))
    (get-background-mult
     (type: (-> wxmultcolour-object)))
    (collapse (type: (wxstyledelta-object -> bool)))
    (get-foreground-mult
     (type: (-> wxmultcolour-object)))
    (get-underlined-off
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-underlined-on
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-style-off
     (union (type: (num -> void)) (type: (-> num))))
    (get-style-on
     (union (type: (num -> void)) (type: (-> num))))
    (get-family
     (union (type: (num -> void)) (type: (-> num))))
    (get-weight-off
     (union (type: (num -> void)) (type: (-> num))))
    (get-weight-on
     (union (type: (num -> void)) (type: (-> num))))
    (copy (type: (wxstyledelta-object -> void)))
    (get-size-add
     (union (type: (num -> void)) (type: (-> num))))
    (get-size-mult
     (union (type: (num -> void)) (type: (-> num))))
    (equal? (type: (wxstyledelta-object -> bool)))
    (get-alignment-off
     (union (type: (num -> void)) (type: (-> num))))
    (get-face
     (union (type: (str -> void)) (type: (-> str))))
    (get-transparent-text-backing-off
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-alignment-on
     (union (type: (num -> void)) (type: (-> num))))
    (set-delta
     (type: (num optional num -> wxstyledelta-object)))
    (get-background-add
     (type: (-> wxaddcolour-object)))
    (set-delta-face
     (type: (str -> wxstyledelta-object)))
    (set-delta-background
     (union (type: (str -> wxstyledelta-object))
      (type: (wxcolour-object -> wxstyledelta-object)))))))

 (define wx:text-snip%
  (wxr:class* this
   ((super wx:snip%))
   ((init1 (type: num)))
   (public
    (get-text (type: (num num optional bool -> str)))
    (size-cache-invalid (type: (-> void)))
    (on-event
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxmouseevent-object
	     ->
	     void)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (do-font (type: (num optional bool -> void)))
    (set-admin (type: (wxsnipadmin-object -> void)))
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
    (match? (type: (wxsnip-object -> bool)))
    (insert (type: (str num optional num -> void)))
    (copy (type: (-> wxsnip-object)))
    (resize (type: (num num -> bool)))
    (partial-offset
     (type: (wxdc-object num num num -> num)))
    (merge-with
     (type: (wxsnip-object -> wxsnip-object)))
    (write (type: (wxmediastreamout-object -> void)))
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
    (read (type: (num wxmediastreamin-object -> void)))
    (on-char
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxkeyevent-object
	     ->
	     void)))
    (own-caret (type: (bool -> void))))))

 (define wx:add-colour%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-b (union (type: (num -> void)) (type: (-> num))))
    (get (type: ((box num) (box num) (box num) -> void)))
    (get-g (union (type: (num -> void)) (type: (-> num))))
    (set (type: (num num num -> void)))
    (get-r (union (type: (num -> void)) (type: (-> num)))))))

 (define wx:tab-snip%
  (wxr:class* this
   ((super wx:text-snip%))
   ()
   (public
    (get-text (type: (num num optional bool -> str)))
    (size-cache-invalid (type: (-> void)))
    (on-event
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxmouseevent-object
	     ->
	     void)))
    (do-edit
     (type: (num optional bool optional num -> void)))
    (do-font (type: (num optional bool -> void)))
    (set-admin (type: (wxsnipadmin-object -> void)))
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
    (match? (type: (wxsnip-object -> bool)))
    (copy (type: (-> wxsnip-object)))
    (resize (type: (num num -> bool)))
    (partial-offset
     (type: (wxdc-object num num num -> num)))
    (merge-with
     (type: (wxsnip-object -> wxsnip-object)))
    (write (type: (wxmediastreamout-object -> void)))
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
    (on-char
     (type: (wxdc-object
	     num
	     num
	     num
	     num
	     wxkeyevent-object
	     ->
	     void)))
    (own-caret (type: (bool -> void))))))

 (define wx:buffer-data-class-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (find (type: (str -> (union null wxbufferdataclass-object))))
    (add (type: (wxbufferdataclass-object -> void)))
    (nth (type: (num -> (union null wxbufferdataclass-object))))
    (find-position
     (type: (wxbufferdataclass-object -> num)))
    (number (type: (-> num))))))

 (define wx:mult-colour%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-b (union (type: (num -> void)) (type: (-> num))))
    (get (type: ((box num) (box num) (box num) -> void)))
    (get-g (union (type: (num -> void)) (type: (-> num))))
    (set (type: (num num num -> void)))
    (get-r (union (type: (num -> void)) (type: (-> num)))))))

 (define wx:media-canvas%
  (wxr:class* this
   ((super wx:canvas%))
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
    (scroll
     (union (type: (num num bool -> void))
      (type: (num num -> void))))
    (on-size (type: (num num -> void)))
    (on-paint (type: (-> void)))
    (pre-on-event
     (type: (wxwindow-object wxmouseevent-object -> bool)))
    (on-event (type: (wxmouseevent-object -> void)))
    (set-media
     (type: ((union null wxmediabuffer-object)
	     optional
	     bool
	     ->
	     void)))
    (get-media
     (type: (-> (union null wxmediabuffer-object))))
    (is-focus-on? (type: (-> bool)))
    (force-display-focus (type: (bool -> void)))
    (allow-scroll-to-last (type: (bool -> void)))
    (get-lazy-refresh (type: (-> bool)))
    (on-kill-focus
     (union (type: (-> void)) (type: (-> void))))
    (set-lazy-refresh (type: (bool -> void)))
    (on-set-focus
     (union (type: (-> void)) (type: (-> void))))
    (call-as-primary-owner (type: ((-> _) -> empty)))
    (pre-on-char
     (type: (wxwindow-object wxkeyevent-object -> bool)))
    (on-char (type: (wxkeyevent-object -> void))))))

 (define wx:command-event%
  (wxr:class* this
   ((super wx:event%))
   (init1-arg-num)
   (public
    (get-command-string
     (union (type: (str -> void)) (type: (-> str))))
    (is-selection? (type: (-> bool)))
    (get-selection (type: (-> num)))
    (get-string (type: (-> str)))
    (get-command-int
     (union (type: (num -> void)) (type: (-> num))))
    (get-extra-long
     (union (type: (num -> void)) (type: (-> num))))
    (checked? (type: (-> bool))))))

 (define wx:media-admin%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (set-cursor (type: (wxcursor-object -> void)))
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
    (resized (type: (bool -> void)))
    (grab-caret (type: (-> void)))
    (needs-update (type: (num num num num -> void)))
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
	     void))))))

 (define wx:mouse-event%
  (wxr:class* this
   ((super wx:event%))
   (init1-arg-num)
   (public
    (get-meta-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (entering? (type: (-> bool)))
    (get-control-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (leaving? (type: (-> bool)))
    (get-shift-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (is-button? (type: (-> bool)))
    (get-y (union (type: (num -> void)) (type: (-> num))))
    (get-x (union (type: (num -> void)) (type: (-> num))))
    (button? (type: (num -> bool)))
    (moving? (type: (-> bool)))
    (get-right-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-middle-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (get-left-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (button-d-click? (type: (optional num -> bool)))
    (button-down? (type: (optional num -> bool)))
    (get-time-stamp
     (union (type: (num -> void)) (type: (-> num))))
    (button-up? (type: (optional num -> bool)))
    (get-alt-down
     (union (type: (bool -> void)) (type: (-> bool))))
    (dragging? (type: (-> bool))))))

 (define wx:post-script-dc%
  (wxr:class* this
   ((super wx:dc%))
   (init1-arg-str (init2 (type: bool)))
   (public)))

 (define wx:meta-file-dc%
  (wxr:class* this
   ((super wx:dc%))
   ((init1 (type: str)))
   (public)))

 (define wx:bitmap%
  (wxr:class* this
   ((super wx:object%))
   (|init1-arg-(listof char)-or-init1-arg-num-or-init1-arg-str|
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num)))
   (public
    (get-height (type: (-> num)))
    (save-file
     (type: (str num
	     optional
	     (union null wxcolourmap-object)
	     ->
	     void)))
    (get-width (type: (-> num)))
    (ok? (type: (-> bool)))
    (load-file (type: (str optional num -> void)))
    (set-colour-map
     (type: ((union null wxcolourmap-object) -> void)))
    (get-depth (type: (-> num))))))

 (define wx:menu-bar%
  (wxr:class* this
   ((super wx:object%))
   (|init1-arg-(listof wxmenu-object)|
 |init2-arg-(listof str)|)
   (public
    (enable (type: (num bool -> void)))
    (enable-top (type: (num bool -> void)))
    (set-title (type: (str -> void)))
    (get-label (type: (num -> str)))
    (find-menu-item (type: (str str -> num)))
    (delete
     (type: (wxmenu-object optional num -> void)))
    (get-help-string (type: (num -> str)))
    (append (type: (wxmenu-object str -> void)))
    (get-label-top (type: (num -> str)))
    (set-help-string (type: (num str -> void)))
    (check (type: (num bool -> void)))
    (set-label (type: (num str -> void)))
    (checked? (type: (num -> bool)))
    (set-label-top (type: (num str -> void)))
    (get-title (type: (-> str))))))

 (define wx:font-list%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (remove-font (type: (wxfont-object -> void)))
    (find-or-create-font
     (union (type: (num num num num optional bool -> wxfont-object))
      (type: (num str
	      num
	      num
	      num
	      optional
	      bool
	      ->
	      wxfont-object)))))))

 (define wx:button-bar%
  (wxr:class* this
   ((super wx:tool-bar%))
   (init1-arg-wxframe-object
    (init2 (type: num))
    (init3 (type: num))
    (init4 (type: num))
    (init5 (type: num))
    (init6 (type: num))
    (init7 (type: num))
    (init8 (type: num)))
   (public
    (set-default-size (type: (num num -> void)))
    (get-default-button-width (type: (-> num)))
    (get-default-button-height (type: (-> num))))))

 (define wx:buffer-data%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (write (type: (wxmediastreamout-object -> bool)))
    (get-dataclass
     (union (type: ((union null wxbufferdataclass-object) -> void))
      (type: (-> (union null wxbufferdataclass-object)))))
    (get-next
     (union (type: ((union null wxbufferdata-object) -> void))
      (type: (-> (union null wxbufferdata-object))))))))

 (define wx:check-box%
  (wxr:class* this
   ((super wx:item%))
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
    (get-value (type: (-> bool)))
    (set-value (type: (bool -> void)))
    (set-label
     (union (type: (wxbitmap-object -> void))
      (type: (str -> void)))))))

 (define wx:tool-bar-tool%
  (wxr:class* this ((super wx:canvas%)) () (public)))

 (define wx:buffer-data-class%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (read (type: (wxmediastreamin-object
		  ->
		  (union null wxbufferdata-object))))
    (get-classname
     (union (type: (str -> void)) (type: (-> str))))
    (get-required
     (union (type: (bool -> void)) (type: (-> bool)))))))

 (define wx:media-pasteboard%
  (wxr:class* this
   ((super wx:media-buffer%))
   ()
   (public
    (move (union (type: (wxsnip-object num num -> void))
	   (type: (num num -> void))))
    (resize (type: (wxsnip-object num num -> bool)))
    (lower (type: (wxsnip-object -> void)))
    (find-snip
     (type: (num num -> (union null wxsnip-object))))
    (on-insert
     (type: (wxsnip-object wxsnip-object num num -> bool)))
    (refresh (type: (num num num num bool -> void)))
    (insert
     (union (type: (wxsnip-object -> void))
      (union (type: (wxsnip-object num num -> void))
       (union (type: (wxsnip-object
		      (union null wxsnip-object)
		      ->
		      void))
	(type: (wxsnip-object
		(union null wxsnip-object)
		num
		num
		->
		void))))))
    (erase (type: (-> void)))
    (after-move-to
     (type: (wxsnip-object num num bool -> void)))
    (copy (type: (optional bool optional num -> void)))
    (after-resize
     (type: (wxsnip-object num num bool -> void)))
    (kill (type: (optional num -> void)))
    (do-copy (type: (num bool -> void)))
    (on-move-to
     (type: (wxsnip-object num num bool -> bool)))
    (raise (type: (wxsnip-object -> void)))
       (read-from-file
        (type: (wxmediastreamin-object -> bool)))
       (write-footers-to-file
        (type: (wxmediastreamout-object -> bool)))
       (write-to-file
        (type: (wxmediastreamout-object -> bool)))
       (on-default-event
        (type: (wxmouseevent-object -> void)))
       (read-header-from-file
        (type: (wxmediastreamin-object str -> bool)))
       (get-center
        (type: ((box num) (box num) -> void)))
       (cut (type: (optional bool optional num -> void)))
       (resized (type: (wxsnip-object bool -> void)))
       (on-focus (type: (bool -> void)))
       (delete
        (union (type: (-> void))
	 (type: (wxsnip-object -> void))))
       (do-paste (type: (num -> void)))
       (on-change (type: (-> void)))
       (paste (type: (optional num -> void)))
       (write-headers-to-file
        (type: (wxmediastreamout-object -> bool)))
       (get-snip-data
        (type: (wxsnip-object
		->
		(union null wxbufferdata-object))))
       (change-style
        (union (type: ((union null wxstyledelta-object) -> void))
	 (union (type: ((union null wxstyledelta-object)
			(union null wxsnip-object)
			->
			void))
	  (type: ((union null wxstyle-object)
		  optional
		  (union null wxsnip-object)
		  ->
		  void)))))
       (no-selected (type: (-> void)))
       (on-save-file (type: (str num -> bool)))
       (set-modified (type: (bool -> void)))
       (set-caret-owner
        (type: ((union null wxsnip-object) -> void)))
       (load-file (type: (optional str -> bool)))
       (set-before
        (type: (wxsnip-object
		(union null wxsnip-object)
		->
		void)))
       (on-local-char
        (type: (wxkeyevent-object -> void)))
       (set-after
        (type: (wxsnip-object
		(union null wxsnip-object)
		->
		void)))
       (release-snip (type: (wxsnip-object -> bool)))
       (save-file (type: (optional str -> bool)))
       (set-selected (type: (wxsnip-object -> void)))
       (on-new-image-snip
        (type: (str num bool bool -> wximagesnip-object)))
       (after-insert
        (type: (wxsnip-object wxsnip-object num num -> void)))
       (add-selected
        (union (type: (wxsnip-object -> void))
	 (type: (num num num num -> void))))
       (on-char (type: (wxkeyevent-object -> void)))
       (on-delete (type: (wxsnip-object -> bool)))
       (on-new-box (type: (num -> wxsnip-object)))
       (after-delete (type: (wxsnip-object -> void)))
       (remove (type: (wxsnip-object -> void)))
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
       (scroll-to
        (type: (wxsnip-object num num num num bool -> bool)))
       (find-first-snip
        (type: (-> (union null wxsnip-object))))
       (is-selected?
        (type: ((union null wxsnip-object) -> bool)))
       (needs-update
        (type: (wxsnip-object num num num num -> void)))
       (own-caret (type: (bool -> void)))
       (find-next-selected-snip
        (type: ((union null wxsnip-object)
                ->
                (union null wxsnip-object))))
       (on-resize
        (type: (wxsnip-object num num -> bool)))
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
       (size-cache-invalid (type: (-> void)))
       (after-save-file (type: (bool -> void)))
       (on-local-event
        (type: (wxmouseevent-object -> void)))
       (on-load-file (type: (str num -> bool)))
       (on-double-click
        (type: (wxsnip-object wxmouseevent-object -> void)))
       (after-load-file (type: (bool -> void)))
       (set-snip-data
        (type: (wxsnip-object
		(union null wxbufferdata-object)
		->
		void)))
       (get-file (type: (str -> str)))
       (read-footer-from-file
        (type: (wxmediastreamin-object str -> bool)))
       (put-file (type: (str str -> str)))
       (add-pasteboard-functions
        (type: (wxkeymap-object -> void)))
       (on-event (type: (wxmouseevent-object -> void)))
       (get-flattened-text (type: (-> str)))
       (on-default-char
        (type: (wxkeyevent-object -> void)))
       (move-to (type: (wxsnip-object num num -> void)))
       (set-filename
        (type: (str optional bool -> void))))))

 (define wx:font-name-directory%
  (wxr:class* this
   ((super wx:object%))
   ()
   (public
    (get-post-script-name
     (type: (num num num -> str)))
    (get-font-name (type: (num -> str)))
    (get-afm-name (type: (num num num -> str)))
    (find-or-create-font-id (type: (str num -> num)))
    (get-new-font-id (type: (-> num)))
    (get-family (type: (num -> num)))
    (get-screen-name (type: (num num num -> str)))
    (initialize (type: (num num str -> void)))
    (get-font-id (type: (str -> num))))))

 (define wx:icon%
  (wxr:class* this     
   ((super wx:object%))
   (init1-arg-str (init2 (type: num)))
   (public (ok? (type: (-> bool)))))))
