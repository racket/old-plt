(module tester-view mzscheme
  (require (lib "mred.ss" "mred"))  
  (require (lib "class.ss"))
  (require (lib "thread.ss"))
  (require (lib "framework.ss" "framework"))
  (require (lib "hierlist.ss" "hierlist"))
  (require (lib "etc.ss"))
  
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
      (define selectable-tests (make-object hierarchical-list% selection-mode-panel))
      
      (define sel-buttons-panel (make-object horizontal-panel% selection-mode-panel))
      (define okay-button (make-object button% "Okay" sel-buttons-panel okay-press))
      (define select-all-button (make-object button% "Select All" sel-buttons-panel (lambda (x y) (select-all))))
      ; set-selections : (listof (list str val (listof (cons (list str val) (list str val)))))
      (define set-selections
        (lambda (selection-list)
          (for-each
           (lambda (test-group)
             (let ((tg (send selectable-tests new-list (list-mixin (car test-group)
                                                                   (cadr test-group)))))
               (send (send tg get-editor) insert (car test-group))
               (for-each
                (lambda (test)
                  (let* ([i (send tg new-item (item-mixin (car test) (cadr test) tg))]
                         [ed (send i get-editor)])
                    (send ed insert (car test) 0 'same)
                    (send ed set-clickback 0 (string-length (car test))
                          (lambda (a b c) (send i toggle-selectedness)))))
                (caddr test-group))))
           selection-list)))
      
      (define (select-all)
        (for-each (lambda (x) (send x select-all) (send selectable-tests get-items))
                  (send selectable-tests get-items)))

      ; get-current-test-selections : -> (listof (list str (cons str (listof str))))
      (define (get-current-test-selections)
        (define (f l)
          (cond
            [(null? l) null]
            [else
             (if (send (car l) has-any-selections?)
                 (cons (send (car l) to-selection)
                       (f (cdr l)))
                 (f (cdr l)))]))
        (f (send selectable-tests get-items)))
      
      (send sel-buttons-panel stretchable-height #f)
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
      
      (super-instantiate (#f 1 parent '(horizontal)))))
  
  ; str -> (class -> list-mixin)
  ; our new lists know how many selected "kids" they have (but not what they
  ; are or how many unselected) -- this allows us to boldify 
  (define list-mixin 
    (lambda (name val)
      (lambda (superclass)
        (class superclass ()
          (public to-selection 
                  select-all
                  increment-selected-kids
                  decrement-selected-kids
                  has-any-selections?)
          (init-rest args)
          
          (define selected-style (make-object style-delta% 'change-bold))
          (define unselected-style (make-object style-delta% 'change-normal))
          (define selected-kids 0)
          
          ; -> str
          ; return the selection representation of this list
          (define (to-selection)
            (define (f l)
              (cond
                [(null? l) null]
                [else
                 (if (send (car l) am-i-selected?)
                     (cons (send (car l) to-selection)
                           (f (cdr l)))
                     (f (cdr l)))]))
            (list val (f (send this get-items))))
          
          ; -> void
          (define (select-all)
            (for-each (lambda (x) (send x select-me)) (send this get-items)))
          
          ; -> void
          ; tell the list about one more kid, possibly changing selected status
          (define (increment-selected-kids)
            (begin
              (set! selected-kids (add1 selected-kids))
              (if (= selected-kids 1) (select-myself))))
          
          ; -> void
          (define (decrement-selected-kids)
            (begin
              (set! selected-kids (sub1 selected-kids))
              (if (= selected-kids 0) (unselect-myself))))
          
          ; style-delta<%> -> void
          ; changes the text style of the label for this snip
          (define (set-style style)
            (let ((ed (send this get-editor)))
              (send ed change-style style 0 (string-length (send ed get-text)))))
          
          (define (has-any-selections?) (> selected-kids 0))
          (define (select-myself) (set-style selected-style))
          (define (unselect-myself) (set-style unselected-style))
          
          (apply super-make-object args)))))
  
  ; item-mixin : str x list-mixin -> (class -> item-mixin)
  (define item-mixin 
    (lambda (str val parent)
      (lambda (superclass)
        (class superclass ()
          (public toggle-selectedness 
                  to-selection 
                  am-i-selected?
                  select-me
                  unselect-me)
          (init-rest vars)
          
          (define selected #f)
          (define selected-style (make-object style-delta% 'change-bold))
          (define unselected-style (make-object style-delta% 'change-normal))
          
          ; -> void
          (define (toggle-selectedness)
            (if selected
                (unselect-me)
                (select-me)))
          
          ; style-delta<%> -> void
          (define (set-style style)
            (let ((ed (send this get-editor)))
              (send ed change-style style 0 (string-length (send ed get-text)))))
          
          (define (unselect-me)
            (set! selected #f)
            (set-style unselected-style)
            (send parent decrement-selected-kids))
          
          (define (select-me)
            (set! selected #t)
            (set-style selected-style)
            (send parent increment-selected-kids))
          
          (define (to-selection) val)
          (define (am-i-selected?) selected)
          
          (apply super-make-object vars)))))  
  
  
  
  )

