(module tester-view mzscheme
  (require (lib "mred.ss" "mred"))  
  (require (lib "class.ss"))
  (require (lib "thread.ss"))
  (require (lib "framework.ss" "framework"))
  
  (provide view%)

(define-struct editor (panel text))
(define view%
  (class* object% ()
    
    (public 
      get-gui-thread
      set-loading-gauge-size
      tick-loading-gauge
      update-load-text
      set-test-text
      get-current-test-selections
      update-status
      set-mode
      set-selections
      set-overall-gauge-size
      set-this-group-gauge-size
      tick-testing-gauges
      init)
    
    (init-field okay-press
                left-button-press
                right-button-press)
    
    (define get-editor
      (lambda (parent-panel)
        (let ((ep (make-object editor-canvas% parent-panel))
              (txt (make-object text%)))
          (begin
            (send txt auto-wrap #t)
            (send ep set-editor txt)
            (make-editor ep txt)))))
    
    ;; --------------------- overall frame ------------------------------
    (define my-eventspace (make-eventspace))
    (define top-frame 
      (parameterize ([current-eventspace my-eventspace])
        (make-object 
            (class* frame% ()
              (override on-close)
              (define (on-close)
                (kill-thread gui-thread))
              
              (super-make-object "Tester" #f 600 400)))))
    
    (define menu-bar  (make-object menu-bar% top-frame))
    (define menu-edit (make-object menu% "Edit" menu-bar))
    (define top-panel (make-object panel:single% top-frame))
    
    ;; --------------------- loading mode -------------------------------
    (define loading-mode-panel (make-object vertical-panel% top-panel))
    (define loading-messages (get-editor loading-mode-panel))
    (define loading-gauge (make-object ticking-gauge% loading-mode-panel))
    (send loading-gauge stretchable-height #f)
    
    (define (set-loading-gauge-size size)
      (begin
        (send loading-gauge set-value 0)
        (send loading-gauge set-range size)))
    (define (tick-loading-gauge)
      (send loading-gauge tick))
    
    
    ;; --------------------- choice mode --------------------------------
    (define selection-mode-panel (make-object vertical-panel% top-panel))
    (define sel-list-panel (make-object horizontal-panel% selection-mode-panel))
    (define group-selection-box (make-object list-box%
                                  "Test groups"
                                  null
                                  sel-list-panel
                                  (lambda (x y) (on-group-select x y))
                                  '(single)))
    (define test-selection-box (make-object list-box%
                                 "Tests"
                                 null
                                 sel-list-panel
                                 (lambda (x y) (capture-current-selection))
                                 '(extended)))
    
    (define sel-buttons-panel (make-object horizontal-panel% selection-mode-panel))
    (define okay-button (make-object button% "Okay" sel-buttons-panel okay-press))
    
    ; get-current-test-selections : -> (listof (list natnum (listof natnum)))
    ; returns the list of current selections from the test selection widget
    (define get-current-test-selections
      (lambda ()
        (let loop ((counter 0))
          (cond
            [(>= counter (vector-length current-selected-tests)) null]
            [else
             (let ((line (vector-ref current-selected-tests counter)))
               (if (null? line)
                   (loop (add1 counter))
                   (cons (list counter line) (loop (add1 counter)))))]))))
    
    (define select-all-button (make-object button%
                                "Select all" sel-buttons-panel void))
    (define select-none-button (make-object button%
                                 "Clear all selections" sel-buttons-panel void))
    (send sel-buttons-panel stretchable-height #f)
    
    ; current-selected : vector[(listof natnum)]
    ; holds the currently-selected tests
    (define current-selected-tests (make-vector 0))
    ; current-selected-group : natnum
    ; the index of the currently-selected group
    (define current-selected-group #f)
    
    ; set-selections : (listof (list str (listof str))) -> void
    (define (set-selections new-selections)
      (set! current-selected-tests (make-vector (length new-selections) null))
      (set! current-selected-group (if (null? new-selections) #f 0))
      (update-group-box new-selections))
    
    ; capture-current-selection : -> void
    ; set!'s the current selected group's current selected tests to reflect the actual
    ; selection in the test box
    (define (capture-current-selection)
      (vector-set! current-selected-tests 
                   current-selected-group 
                   (send test-selection-box get-selections)))
    
    ; update-group-box : (listof str (listof str)) -> void
    (define (update-group-box sel-list)
      (set-listbox-contents group-selection-box sel-list))
    
    ; on-group-select : list-box<%> x callback-event% -> void
    ; handles a click on the left menu box (controls remembering what was selected
    ; in the right-hand side)
    (define (on-group-select listbox callback)
      (if (eq? (send callback get-event-type) 'list-box)
          (let ([sel (send group-selection-box get-selections)])
            (if (not (null? sel))
                (let ((sel (car sel)))
                  (begin
                    (update-test-box (send group-selection-box get-data sel)
                                     (vector-ref current-selected-tests sel))
                    (set! current-selected-group sel)))))))
    
    ; update-test-box : (listof str) x (listof natnum) -> void
    (define (update-test-box vals selections)
      (begin 
        (send test-selection-box set vals)
        (for-each (lambda (selection) (send test-selection-box select selection #t)) selections)))
    
    ; list-box<%> x (listof (list str val)) -> void
    ; sets the list-box's contents to the given list and selects the first item
    (define (set-listbox-contents box val-list)
      (begin
        (send box set '())
        (for-each
         (lambda (x) (send box append (car x) (cadr x)))
         val-list)))
       
    (send sel-buttons-panel set-alignment 'center 'center)
    
    ;; --------------------- testing mode -------------------------------
    (define testing-mode-panel (make-object vertical-panel% top-panel))
    (define testing-message    (get-editor testing-mode-panel))
    (define testing-button-panel (make-object horizontal-panel% 
                                   testing-mode-panel))
    (define prev-error-button (make-object button% 
                                "Previous error" testing-button-panel left-button-press))
    (define next-error-button (make-object button% 
                                "Next error" testing-button-panel right-button-press))
    (define testing-overall-gauge (make-object ticking-gauge% testing-mode-panel))
    (define testing-this-group-gauge (make-object ticking-gauge% testing-mode-panel)) 
    (send testing-button-panel stretchable-height #f)
    
    ; set-overall-gauge-size : natnum -> void
    ; resets the overall gauge and makes its range the given number
    ; also resets the this-group gauge and sets its range to 1
    (define (set-overall-gauge-size size)
      (begin
        (send testing-overall-gauge set-value 0)
        (send testing-overall-gauge set-range size)
        (set-this-group-gauge-size 1)))
    
    ; set-this-group-gauge-size : natnum -> void
    (define (set-this-group-gauge-size size)
      (begin
        (send testing-this-group-gauge set-value 0)
        (send testing-this-group-gauge set-range size)))
    (define (tick-testing-gauges)
      (send testing-overall-gauge tick)
      (send testing-this-group-gauge tick))
   
    ;; -------------------- misc private fields --------------------------
    (define gui-thread (parameterize ([current-eventspace my-eventspace])
                         (current-thread)))
    
    ; mode-list : (listof (list sym [child of top-panel]))
    ; associates a mode-name with a list of panels to be visible
    (define mode-list `((load ,loading-mode-panel)
                        (choose ,selection-mode-panel)
                        (test ,testing-mode-panel)))
    
    ; set-mode : [symbol listed in list mode-list] -> void
    ; side effect : switches the gui into the named mode
    (define set-mode
      (lambda (mode)
        (let ((new-panel (assq mode mode-list)))
          (if new-panel
              (send top-panel active-child (cadr new-panel))
              (raise-mismatch-error 'set-mode 
                                    (format "Expects symbol in ~v, given " (map car mode-list)) 
                                    mode)))))
    
    ;; get-gui-thread : -> thread-id
    ;; returns the id of the thread on which the gui is running
    (define (get-gui-thread) gui-thread)

    ;; set-txt : struct:editor x str -> void
    ;; clears the current message text and replaces it with the given string
    (define set-txt
      (lambda (text-ed txt)
        (let ((text-obj (editor-text text-ed)))
          (send text-obj erase)
          (send text-obj insert txt))))

    ;; update-load-text : str -> void
    ;; gives the GUI a new message for its single window
    (define update-load-text
      (lambda (str)
        (send (editor-text loading-messages) insert (format "~a~n" str))))

    ;; set-test-text : str -> void
    ;; sets the text in the test message window to a particular value
    (define set-test-text
      (lambda (txt)
        (set-txt testing-message txt)))
    
    ;; get-thread : -> thread
    ;; returns the gui's thread ID
    (define get-thread
      (lambda () (current-thread)))
    
    ;; update-status : str -> void
    ;; updates the status bar with the given message
    (define update-status
      (lambda (message) (send top-frame set-status-text message)))

    ;; init : -> void
    ;; initializes the window
    (define init
      (lambda ()
        (send top-frame create-status-line)
        (append-editor-operation-menu-items menu-edit)
        (send this set-mode 'load)
        (send top-frame show #t)))
    
    (super-instantiate ())
    
    (send this init)))
  
(define ticking-gauge%
  (class* gauge% ()
    (public tick)
    (init-field parent)
    (inherit get-value set-value)
    
    (define tick (lambda () (set-value (add1 (get-value)))))
   
    (super-instantiate (#f 1 parent '(horizontal))))))