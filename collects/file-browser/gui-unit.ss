(module gui-unit mzscheme
  
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "list.ss")
           "sigs.ss"
           "weak-set.ss"
           "widgets.ss")
  
  (provide make-gui@)
  
  (define (make-gui@ state frame)
    (unit/sig gui^
      (import (script : script^) code-engine^)
      
      (define string-file-snip%
        (class string-snip%
          (public get-file select unselect is-selected?)
          (inherit get-admin)
          (init-field file)        
          (let ((str (string-append (script:file-name file) 
                                    (if (script:is-directory? file) "/" ""))))
            (super-instantiate (str)))
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
               (set! selected #f))))))
      
      
      (define file-board%
        (class pasteboard%
          (public selection-updated select unselect)
          (inherit find-snip find-first-snip insert set-selection-visible get-canvas
                   dc-location-to-editor-location remove-selected)
          (super-instantiate ())
          (init files selection)
          
          (set-selection-visible #f)
          
          (define snip-table (make-hash-table 'equal))
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
          
          (define/override (after-select snip on?) 
            (cond
              ((and on? (not (script:is-directory? (send snip get-file))))
               (if (send snip is-selected?)
                   (script:remove-selection (send snip get-file))
                   (script:cons-selection (send snip get-file)))
               (remove-selected snip))))

          
          (rename (super.on-event on-event))
          (define/override (on-event event)
            (let-values (((x y) (dc-location-to-editor-location (send event get-x)
                                                                (send event get-y))))
              (let ((snip (find-snip x y #f)))
                (cond
                  (snip
                   (cond
                     ((eq? (send event get-event-type) 'left-down)
                      (snip-select-toggle snip))))
                  (else
                   (super.on-event event))))))
          
          (define (dir-clicked file)
            (send (send (get-canvas) get-parent) dir-clicked file))
          
          (define (snip-select-toggle snip)
            (let ((file (send snip get-file)))
              (cond
                ((script:is-directory? file)
                 (dir-clicked file))
                (else
                 (cond
                   ((send snip is-selected?)
                    (script:remove-selection file))
                   (else 
                    (script:cons-selection file)))))))
          (for-each (lambda (x) (send this select x)) selection)))
      
      (define file-window%
        (class vertical-panel%
          (public selection-updated select unselect files-changed file-added file-deleted
                  dir-clicked activate)

          ;; dir: file
          (init-field dir)

          (super-instantiate () (style '(border)))
                    
          (inherit set-label)
          (set-label (script:file-full-path dir))
          (send window-pane set-button-label this (script:file-full-path dir))
          
          (define file-filter (get-user-value 'all-files))
          (define file-sort (get-user-value 'dirs-first))

          (define (activate)
            (script:set-current-dir! dir))
          
          (define (change-dir new-dir)
            (send path-text set-value (script:file-full-path new-dir))
            (history-add dir)
            (send window-pane set-button-label this (script:file-full-path new-dir))
            (set! dir new-dir)
            (refresh))
          (define (dir-clicked file)
            (change-dir file))
          
          (define history null)
          (define (history-add dir)
            (set! history (cons dir history)))
          (define (history-back)
            (if (not (null? history))
                (begin
                  (dir-clicked (car history))
                  (set! history (cddr history)))))
          
          (define (get-restricted-selection)
            (filter (lambda (f) (script:file=? (script:file-dir f) dir)) (script:get-selection)))
          
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
            (let ((files
                   (quicksort (filter file-filter (script:directory-list dir))
                              file-sort)))
              (set! file-pasteboard (make-object file-board% 
                                      files (get-restricted-selection)))
              (send file-canvas set-editor file-pasteboard)))
          
          (define (callback widget event)
            (cond
              ((eq? (send event get-event-type) 'button)
               (cond
                 ((eq? widget back-button)
                  (history-back))
                 ((eq? widget up-button)
                  (dir-clicked (script:file-dir dir)))))))
          
          (define toolbar (instantiate horizontal-panel% (this) (stretchable-height #f)))
          (define back-button (make-object button% "back" toolbar callback))
          (define up-button (make-object button% "up" toolbar callback))
          
          (define path-text
            (make-object commit-text-field%
              (lambda (path) 
                (let ((file (script:make-file path)))
                  (cond
                    ((script:is-directory? file) file)
                    (else
                     (user-thread-eval (format
                                        "(printf \"No directory ~a~n\")"
                                        path)
                                       void)
                     #f))))
              "path"
              this
              (lambda (path) (change-dir path))
              (script:file-full-path dir)))
          
          
          (define file-canvas (make-object editor-canvas% this))
          (define file-pasteboard #f)
          (refresh)))
      
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
      
      (define (close-window)
        (send window-pane close-current))
      
      (define window-pane (make-object tabbed-panel% frame))
      
      (add-window (script:get-current-dir))
      
      (send frame show #t))))


