;; Tframe.ss - creates MrSpidey frames
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

; Global variable to record the most-recently created frame
(define tf (void))

(define shake-it-repetitions 25)

(define spidey:frame%

  (class frame:searchable%

    (arg-main arg-filename summary-edit . init-locs)

    (inherit 
     show get-menu-bar get-canvas get-canvas%
     get-area-container
     create-status-line set-status-text
     get-size set-icon 
     set-label-prefix
     move resize)

    ;; ----------

    (rename
      [super-on-close on-close])

    ;; ----------

    (override

      [on-close
        (lambda ignored
	  (send main on-frame-close filename)
	  (remq-callback-sdl-alg-changed! flush-type-cache)
	  (send program-edit on-close) ; fixes PR 948
	  (super-on-close)
	  (send this show #f))]
      [file-menu:between-close-and-quit
        (lambda (file-menu)
	  (make-object menu-item% "Close All" file-menu 
		       (lambda (mi ce) 
			 (wrap-busy-cursor 
			  (lambda () 
			    (send main close-all-frames))))))])
      
    (public

      [auto-set-wrap #f]
      [panel #f] 
      [set-show-mode
        (lambda (which)
          (pretty-debug-gui `(set-show-mode ,which))
          (unless (eq? which canvas-show-mode)
            (set! canvas-show-mode which)
            (send summary-canvas stretchable-height (eq? which 'summary))
            (send panel change-children
              (lambda (ignore)
                (filter
                  (lambda (x) x)
                  (list 
                    (and (or (eq? which 'program) (eq? which 'both))
                      program-canvas)
                    (and (or (eq? which 'summary) (eq? which 'both))
                      summary-canvas)))))))]

      ;; ---------- Set up the menus

      [flush-type-cache (lambda () (void))]

      [calc-show
        (lambda ()
          (set-show-mode 'program)
          (when (and 
                  summary-canvas
                  ;; Show summary only if some real content
                  (> (send summary-edit last-line) 3))
            ;;(printf "Summary-edit size ~s~n" (send summary-edit last-line))
            (set-show-mode 'both)))]

      [init-show-menu #f]
      )
    
    (override

     [file-menu:new #f]
     [file-menu:open #f]
     [file-menu:revert #f]
     [file-menu:save #f]
     [file-menu:save-as #f]
     [file-menu:close on-close]

     [file-menu:between-open-and-revert
      (lambda (file-menu)
	(let ([new-item 
	       (lambda (label cb)
		 (make-object menu-item% label file-menu cb))])
	  (new-item "Open..." 
		    (lambda (mi ce) (send main open-analyzed-file-choice)))
	  (new-item "Open All"
		    (lambda (mi ce) (wrap-busy-cursor 
				     (lambda () (send main open-all #t)))))
	  (new-item "Load All" 
		    (lambda (mi ce) (wrap-busy-cursor 
				     (lambda () (send main open-all #f)))))
	  (new-item "Reanalyze" 
		    (lambda (mi ce) (wrap-busy-cursor 
				     (lambda () (send main reanalyze)))))))]

      [file-menu:between-save-as-and-print (lambda args (void))]

      [edit-menu:undo #f]   
      [edit-menu:redo #f]
      [edit-menu:cut #f]
      [edit-menu:paste #f]

      [edit-menu:between-find-and-preferences (lambda args (void))])

    ;; ----------

    (public
     [main arg-main]                   ; parent containing the global state
      program-canvas
      program-edit
      summary-canvas                   ; or #f if no summary
      [filename arg-filename]

      [canvas-show-mode 'none]          ; 'program, 'summary, or 'both
      [display-mode  (car modes)]       ; which display mode

      [set-display-mode
        (lambda (which)
          (set! display-mode which)
          (pretty-debug-gui `(set-display-mode ,display-mode ,filename))
          ;; Call main to create a new edit buffer,
          ;; and to load and annotate file
          (set! program-edit 
            (send main annotated-edit display-mode filename program-canvas))
          (send program-canvas set-editor program-edit))]

      [focus-def
        (lambda (pos)
          (unless (memq display-mode '(program both))
            (set-show-mode 'both))
          (let* ( [real-pos (send program-edit real-start-position pos)]
                  [end (scheme-paren:forward-match 
                         program-edit real-pos 
                         (send program-edit last-position))])
            (thread
              (lambda ()
                (sleep)
                (send program-canvas set-focus)
                (send program-edit
                  set-position-bias-scroll -1 real-pos end)))))]

      [shake-it
        (lambda ()
          (send program-edit shake-it))]

      ;; ----------

      )
	
    (sequence 
      (pretty-debug-gui
        `(Tframe ,arg-main ,arg-filename ,summary-edit ,@init-locs))
      (match init-locs
        [(w h x y) 
          (pretty-debug-gui 
	   `(begin (send this move ,(+ x 15) ,(+ y 15))
		   (send this resize ,w ,h)))
	  (move x y)
	  (resize w h)]
        [() (void)])
      (pretty-debug-gui `(Tframe super-init))
      (let ([t (format "~a: ~a" 
                 st:name (file-name-from-path arg-filename))]) 
        (super-init t)
        (pretty-debug-gui `(Tframe super-init done))
        (set-label-prefix t)))

    (sequence
      (set! panel (get-area-container)))

    (sequence
      (let* ([menu-bar (get-menu-bar)]
	     [show-menu (make-object menu% "Show" menu-bar)]
	     [clear-menu (make-object menu% "Clear" menu-bar)]
	     [filter-menu (make-object menu% "Filter" menu-bar)]
	     [check-program #f]
	     [check-summary #f]
	     [check-both #f]
	     [check-alist #f]
	     [show-callback 
	      (lambda (item _)
		(for-each
		 (lambda (c) 
		   (let ([check-item (car c)])
		     (if (eq? check-item item)
			 (set-show-mode (cadr c))
			 (send check-item check #f))))
		 check-alist))]
	     [add-check (lambda (s) (make-object checkable-menu-item% 
						 s show-menu show-callback))]
	     [add-clear-item (lambda (s f) (make-object menu-item% 
							s clear-menu f))]
	     [filters (analysis-get-filters)]
	     [filter-items '()])

	(set! check-program (add-check "Program Only"))
	(set! check-summary (add-check "Summary Only"))
	(set! check-both (add-check "Both"))

	(set! check-alist `((,check-program program)
			    (,check-summary summary)
			    (,check-both both)))

	(send check-program check #t)

	(set-show-mode canvas-show-mode)

	(add-clear-item "Arrows+Types"  
			(lambda (mi ce) 
			  (wrap-busy-cursor 
			   (lambda () 
			     (send* program-edit 
				    (delete-arrows)
				    (delete-types))))
			  "Removes both types and arrows from the window"))

	(add-clear-item "Arrows"  
			(lambda (mi ce) 
			  (wrap-busy-cursor 
			   (lambda () 
			     (send program-edit delete-arrows)))))

	(add-clear-item "Types"  
			(lambda (mi ce) 
			  (wrap-busy-cursor 
			   (lambda () 
			     (send program-edit delete-types)))))

	(for-each
	 (lambda (s)
	   (set! filter-items
		(cons (make-object checkable-menu-item% 
				   s filter-menu 
				   (lambda (item _)
				     (let ([item-label (send item get-label)])
				       (for-each
					(lambda (f)
					  (unless (string=? item-label 
							    (send f get-label))
						  (send f check #f)))
					filter-items)
				       (analysis-set-arrow-filter! 
					(string->symbol item-label)))))
		      filter-items)))
	 filters)

	(set! filter-items (reverse filter-items))
	(send (car filter-items) check #t)
	(analysis-set-arrow-filter! #f))
        (frame:reorder-menus this))

    ;; ---------------------------------------------------------

    (sequence
      (set! flush-type-cache
        (lambda ()
          (pretty-debug-gui '(Tframe flush-type-cache))
          (unless (void? program-edit) 
            (send program-edit flush-type-cache))))

      (add-callback-sdl-alg-changed! flush-type-cache)

      (pretty-debug-gui
        `(Tframe ,arg-main ,arg-filename ,summary-edit ,@init-locs))
      (set! tf this)

      ;; ------------------------------------------------------------
      ;; build the canvases

      (pretty-debug-gui '(setting summary-canvas))
      (set! summary-canvas
        (and summary-edit
          (let ([c (make-object editor-canvas% panel)])
	    (send c set-line-count 5)
	    (send c set-editor summary-edit)
	    c)))

      (assert (is-a? summary-canvas mred:connections-media-canvas%))
      (pretty-debug-gui '(setting program-canvas))
      (set! program-canvas (make-object (get-canvas%) panel))
      (set-display-mode (car modes))
      (pretty-debug-gui '(done setting canvases))

      ;; ------------------------------------------------------------
      ;; install the icon
      
      '(let ([icon (make-object icon% 
                     (build-absolute-path 
                       (collection-path "mrspidey") ; MATTHEW: got rid of plt-home
                       "icon.gif")
                     'gif
                     )])
         (when (send icon ok?) (set-icon icon)))

      ;; ------------------------------------------------------------
      ;; status help line 

      ;;(unless (eq? mred:platform 'macintosh)
      ;;  (create-status-line))

      ;;(set-status-text 
      ;; "Mouse: Left-type/parents  Midde-Ancestors  Right-Close")
      ;; ------------------------------------------------------------

      ;;(set-display-mode display-mode)
      (calc-show)
      (show #t)
      
      )))


