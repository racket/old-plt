; statedit.ss
; Defines spidey:static-edit%, a subclass of mred:media-edit%
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
; ported to MrEd 100 by Paul Steckler 

(define spidey:static-edit%

  (class frame:searchable% init-args

    (inherit get-text-to-search)

    (public

      [begin-edit-sequence-and-unlock
        (lambda ()
          (begin-busy-cursor)
          (send* editor 
		 (lock #f) 
		 (begin-edit-sequence)))]
      [end-edit-sequence-and-lock
        (lambda ()
          (send* editor 
		 (end-edit-sequence)
		 (lock #t))        
	  (end-busy-cursor))]
      [edit-sequence
        (lambda (thunk)
          (dynamic-wind
            begin-edit-sequence-and-unlock
            thunk
            end-edit-sequence-and-lock))]
      [match-paren-forward
        (lambda (source-start)
          (scheme-paren:forward-match
            editor source-start (send editor last-position)))]

      ; editor methods exposed in frame

      [editor-insert 
       (lambda args (apply (ivar editor insert) 
			   args))]
      [editor-position-line 
       (lambda args (apply (ivar editor position-line)
			   args))]
      [editor-change-style 
       (lambda args (apply (ivar editor change-style) 
			   args))]
      [editor-set-clickback 
       (lambda args (apply (ivar editor set-clickback) 
			   args))]
      [editor-flash-on 
       (lambda args (apply (ivar editor flash-on) 
			   args))]
      [editor-set-position 
       (lambda args (apply (ivar editor set-position) 
			   args))]
      [editor-scroll-to-position 
       (lambda args (apply (ivar editor scroll-to-position) 
			   args))]
      [editor-get-text 
       (lambda args (apply (ivar editor get-text) 
			   args))]
      [editor-delete 
       (lambda args (apply (ivar editor delete) 
			   args))]
      [editor-set-tabs 
       (lambda args (apply (ivar editor set-tabs) 
			   args))]
      [editor-last-position 
       (lambda args (apply (ivar editor last-position) 
			   args))]
      [editor-find-wordbreak 
       (lambda args (apply (ivar editor find-wordbreak) 
			   args))]
      [editor-set-wordbreak-map 
       (lambda args (apply (ivar editor set-wordbreak-map) 
			   args))]
      [editor-last-line 
       (lambda args (apply (ivar editor last-line) 
			   args))]
      [editor-line-start-position 
       (lambda args (apply (ivar editor line-start-position) 
			   args))]
      [editor-get-keymap 
       (lambda args (apply (ivar editor get-keymap) 
			   args))]
      [editor-get-start-position 
       (lambda args (apply (ivar editor get-start-position) 
			   args))]
      [editor-get-end-position 
       (lambda args (apply (ivar editor get-end-position) 
			   args))]
      [editor-set-filename 
       (lambda args (apply (ivar editor set-filename) 
			   args))]
      [editor-get-admin 
       (lambda args (apply (ivar editor get-admin) 
			   args))]
      [editor-invalidate-bitmap-cache 
       (lambda args (apply (ivar editor invalidate-bitmap-cache) 
			   args))]
      [editor-position-location
       (lambda args (apply (ivar editor position-location)
			   args))]
      [editor-local-to-global
       (lambda args (apply (ivar editor local-to-global)
			   args))]
      [editor-set-caret-owner
       (lambda args (apply (ivar editor set-caret-owner)
			   args))]
      
      ; initialize editor later		     

      [editor #f])
    
    (override

      [get-editor% (lambda () (scheme:text-mixin text:searching%))])
	      
    (sequence
		
      (apply super-init "foobar" init-args)

      (set! editor (get-text-to-search))

      ;; disable paste for errant right mouse clicks
      (let ([ k (send editor get-keymap)])
        (send k add-function "nothing" (lambda l (void)))
        (send k map-function "middlebutton" "nothing")
        (send k map-function "rightbutton" "nothing"))

      ;; make snips go down instead of up
      ;; oops - can't do this :-(
      '(let ([stan (send (get-style-list) find-named-style "Standard")])
         (when stan (send stan set-delta normal-delta)))

      (send editor lock #t)
      )))
