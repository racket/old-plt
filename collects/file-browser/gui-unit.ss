(module gui-unit mzscheme
  
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           "sigs.ss"
           "weak-set.ss"
           "widgets.ss")
  
  (provide make-gui@)
  
  (define (make-gui@ state)
    (unit/sig gui^
      (import (script : script^) code-engine^)
      
      (define string-file-snip%
        (class string-snip%
          (public get-file select unselect is-selected?)
          (inherit get-admin)
          (init-field file)        
          (define (get-file) file)
          (define selected #f)
          (define (is-selected?) selected)
          (define add-bold
            (make-object style-delta% 'change-bold))
          (define sub-bold
            (make-object style-delta% 'change-normal))
          
          (define (select)
            (cond
              ((not selected)
               (send (send (get-admin) get-editor)
                     change-style add-bold this)
               (set! selected #t))))
          (define (unselect)
            (cond
              (selected
               (send (send (get-admin) get-editor)
                     change-style
                     sub-bold this)
               (set! selected #f))))
          (let ((str (string-append (script:file-name file) 
                                    (if (script:is-directory? file) "/" ""))))
            (super-instantiate (str)))))
      
      
      
      (define file-board%
        (class pasteboard%
          (public selection-updated select unselect)
          (inherit find-snip find-first-snip insert set-selection-visible
                   dc-location-to-editor-location remove-selected get-keymap)
          (init files selection)
          
          (define snip-table (make-hash-table 'equal))
          
          (define (selection-updated selection)
            (unselect-all)
            (for-each (lambda (x) (send this select x)) selection))
          
          (define (select f)
            (send (hash-table-get snip-table (script:file-name f)) select))
          
          (define (unselect f)
            (send (hash-table-get snip-table (script:file-name f)) unselect))
          
          (define (unselect-all)
            (let unselect ((snip (find-first-snip)))
              (cond
                (snip
                 (send snip unselect)
                 (unselect (send snip next))))))
          
          (define last-click-time -inf.0)
          
          (define/override (after-select snip on?) 
            (let ((file (send snip get-file)))
              (cond
                ((and on? (script:is-directory? file))
                 (user-eval `(,box-select-dir ,file) void))
                (on?
                 (user-eval `(,box-select-file ,file) void)))
              (remove-selected snip)))
          
          (rename (super-on-event on-event))
          (define/override (on-event event)
            (let ((event-type (send event get-event-type)))
              (cond
                ((memq event-type '(middle-down right-down left-down))
                 (let-values (((x y) (dc-location-to-editor-location (send event get-x)
                                                                     (send event get-y))))
                   (let ((snip (find-snip x y #f)))
                     (if snip
                         (let ((file (send snip get-file))
                               (keys (list (send event get-alt-down)
                                           (send event get-control-down)
                                           (send event get-meta-down)
                                           (send event get-shift-down))))
                           (cond 
                             ((eq? event-type 'left-down)
                              (let ((click-interval (send (get-keymap) get-double-click-interval))
                                    (click-time (send event get-time-stamp)))
                                (cond
                                  ((< (- click-time last-click-time) click-interval)
                                   (set! last-click-time -inf.0)
                                   (cond
                                     ((script:is-directory? file)
                                      (user-eval `(,double-mouse-dir ,file ',keys) void))
                                     (else
                                      (user-eval `(,double-mouse-file ,file ',keys) void))))
                                  (else
                                   (set! last-click-time click-time)
                                   (cond
                                     ((script:is-directory? file)
                                      (user-eval `(,single-mouse-dir ,file 'left ',keys) void))
                                     (else
                                      (user-eval `(,single-mouse-file ,file 'left ',keys) void)))))))
                             (else
                              (cond
                                ((script:is-directory? file)
                                 (case event-type
                                   ((middle-down) 
                                    (user-eval `(,single-mouse-dir ,file 'middle ',keys) void))
                                   ((right-down) 
                                    (user-eval `(,single-mouse-dir ,file 'right ',keys) void))))
                                (else
                                 (case event-type
                                   ((middle-down) 
                                    (user-eval `(,single-mouse-file ,file 'middle ',keys) void))
                                   ((right-down)
                                    (user-eval `(,single-mouse-file ,file 'right ',keys) void))))))))))))
                (else
                 (super-on-event event)))))
          
          (super-instantiate ())
          (let ((snips (map (lambda (file) (make-object string-file-snip% file)) files)))
            (for-each (lambda (s)
                        (hash-table-put! snip-table 
                                         (script:file-name (send s get-file))
                                         s))
                      snips)
            (let add-snips ((snips snips)
                            (next-x 0)
                            (next-y 0))
              (cond
                ((not (null? snips))
                 (insert (car snips) next-x next-y)
                 (add-snips (cdr snips)
                            (+ 0 next-x)
                            (+ 12 next-y))))))
          (for-each (lambda (x) (select x)) selection)          
          (set-selection-visible #f)))
      
      
      
      
      (define file-window%
        (class vertical-panel%
          (public selection-updated select unselect files-changed file-added file-deleted
                  get-dir history-back change-directory)
          
          ;; dir: file
          (init-field dir)
          (define (get-dir) dir)
          
          (define (change-directory new-dir)
            (send path-text set-value (script:file-full-path new-dir))
            (history-add dir)
            (send window-pane set-button-label this (script:file-full-path new-dir))
            (set! dir new-dir)
            (refresh))
          
          (define history null)
          (define (history-add dir)
            (set! history (cons dir history)))
          (define (history-back)
            (if (not (null? history))
                (begin
                  (change-directory (car history))
                  (set! history (cddr history)))))
          
          (define (get-restricted-selection)
            (filter (lambda (f) (script:file=? (script:file-dir f) dir)) 
                    (script:map-selection (lambda (x) x))))
          
          (define (selection-updated)
            (send file-pasteboard selection-updated (get-restricted-selection)))
          (define (select f)
            (if (script:file=? (script:file-dir f) dir)
                (send file-pasteboard select f)))
          (define (unselect f)
            (if (script:file=? (script:file-dir f) dir)
                (send file-pasteboard unselect f)))
          
          (define (files-changed changed-dir)
            (if (script:file=? changed-dir dir)
                (refresh)))
          (define (file-added file)
            (if (script:file=? (script:file-dir file) dir)
                (refresh)))
          ;(send file-pasteboard file-added file)))
          (define (file-deleted file)
            (if (script:file=? (script:file-dir file) dir)
                (refresh)))
          ;(send file-pasteboard file-deleted file)))
          
          (define (refresh)
            (let ((files (script:directory-list dir)))
              (user-eval `(,quicksort (,filter ,filter-files ',files) ,sort-files)
                         (lambda (files)
                           (set! file-pasteboard (make-object file-board% 
                                                   files (get-restricted-selection)))
                           (send file-canvas set-editor file-pasteboard)))))
          
           (super-instantiate () (style '(border)))
          (inherit set-label)
          (set-label (script:file-full-path dir))
          (send window-pane set-button-label this (script:file-full-path dir))
          
          (define path-text
            (make-object commit-text-field%
              (lambda (path) 
                (let ((file (script:make-file path)))
                  (cond
                    ((script:is-directory? file) file)
                    (else
                     (user-eval `(,error ,(format "No directory ~a~n" path))
                                void)
                     #f))))
              "path"
              this
              (lambda (path) (change-directory path))
              (script:file-full-path dir)))
          
          
          (define file-canvas (make-object editor-canvas% this))
          (define file-pasteboard #f)
          (refresh)))
      
      (define filter-files #f)
      (define sort-files #f)
      (define single-mouse-file #f)
      (define double-mouse-file #f)
      (define single-mouse-dir #f)
      (define double-mouse-dir #f)
      (define box-select-file #f)
      (define box-select-dir #f)
      
      (define (setup-gui sym func)
        (case sym
          ((filter-files) (set! filter-files func))
          ((sort-files) (set! sort-files func))
          ((single-mouse-file) (set! single-mouse-file func))
          ((double-mouse-file) (set! double-mouse-file func))
          ((single-mouse-dir) (set! single-mouse-dir func))
          ((double-mouse-dir) (set! double-mouse-dir func))
          ((box-select-file) (set! box-select-file func))
          ((box-select-dir) (set! box-select-dir func))))
      
      (define file-windows (make-weak-set))
      
      (define (selection-updated)
        (weak-set-for-each! (lambda (fw) (send fw selection-updated))
                            file-windows))
      (define (selection-added f)
        (weak-set-for-each! (lambda (fw) (send fw select f))
                            file-windows))
      (define (selection-removed f)
        (weak-set-for-each! (lambda (fw) (send fw unselect f))
                            file-windows))
      
      (define (add-window dir)
        (let ((new-window (make-object file-window% dir window-pane)))
          (weak-set-add! new-window file-windows)
          (send state register-viewport new-window)))
      
      (define (confirm msg)
        (gui-utils:get-choice (string-append "Really " msg) "yes" "no" "Confirmation" #f))
      
      (define toolbar #f)
          
      (define (toolbar-add label action)
        (make-object button% label toolbar
          (lambda (a b)
            (user-eval `(,action) void))))

      (define (toolbar-spacer)
        (make-object button% "     " toolbar void))
      
      (define (close-window)
        (send window-pane close-current))
      
      (define window-pane #f)
      
      (define (get-current-directory)
        (let ((c (send window-pane get-current)))
          (if c
              (send c get-dir)
              (script:make-file (find-system-path 'home-dir)))))

      (define (change-dir d)
        (let ((c (send window-pane get-current)))
          (if c
              (send c change-directory d))))
              
      (lambda (frame)
        (cond
          ((not window-pane)
           (let ((frame (make-object vertical-panel% frame)))
             (set! toolbar (instantiate horizontal-panel% (frame) (stretchable-height #f)))
             (make-object button% "back" toolbar
               (lambda (a b)
                 (let ((c (send window-pane get-current)))
                   (if c
                       (send c history-back)))))
             (make-object button% "up" toolbar
               (lambda (a b)
                 (let ((c (send window-pane get-current)))
                   (if c
                       (send c change-directory (script:file-dir (send c get-dir)))))))
             (set! window-pane (make-object tabbed-panel% frame)))
           (user-eval-no-disable `(require (file ,(build-path (find-system-path 'pref-dir) ".file-browser.ss")))
                      void)
           (send frame show #t)))))))
