(module tester-view mzscheme
  (require (lib "mred.ss" "mred"))  
  (require (lib "class.ss"))
  (require (lib "thread.ss"))
  
  (provide view%)

(define-struct editor (panel text))  
(define view%
  (class* object% ()
    
    (public 
      update
      set-text
      update-status 
      set-gauge-size 
      set-overall-size
      reset-gauge 
      reset-overall-gauge
      tick-gauge
      get-thread
      single-msgarea-mode
      multi-msgarea-mode
      init)
    
    (init-field left-button-press right-button-press)
    
    (define get-editor
      (lambda ()
        (let ((ep (make-object editor-canvas% message-pane))
              (txt (make-object text%)))
          (begin
            (send txt auto-wrap #t)
            (send ep set-editor txt)
            (make-editor ep txt)))))
    
    (field
     [top-frame  (make-object frame% "Tester" #f 600 400)]
;          (class* frame% ()
;            (override on-close)
;            (define on-close
;              (lambda ()
;                (custodian-shutdown-all (current-custodian))))
;            (super-instantiate ("Tester" #f 600 400))))]
     
     [main-pane      (make-object vertical-pane% top-frame)]
     [message-pane   (make-object vertical-pane% main-pane)]
     [one-message-ed (get-editor)]
     [button-pane    (make-object horizontal-panel% message-pane)]
     [left-button    (make-object button% "Previous error" button-pane left-button-press)]
     [right-button   (make-object button% "Next error" button-pane right-button-press)]
     [gauge-minor    (make-object ticking-gauge% main-pane)]
     [gauge-major    (make-object ticking-gauge% main-pane)]
     [mb             (make-object menu-bar% top-frame)]
     [m-edit         (make-object menu% "Edit" mb)]
     [my-eventspace  (current-eventspace)]
     [lock           (make-semaphore 1)])
    
    ;; single-msgarea-mode : -> void
    ;; switches the gui into single-message-area mode (no next, back buttons)
    (define single-msgarea-mode
      (lambda ()
        (send message-pane change-children
              (lambda (x) (list (editor-panel one-message-ed))))))
    
    ;; multi-msgarea-mode : -> void
    ;; switches the gui into multi-message-area mode (next, back buttons)
    (define multi-msgarea-mode
      (lambda ()
        (send message-pane change-children
              (lambda (x) 
                (list (editor-panel one-message-ed)
                      button-pane)))))
    
    ;; set-txt : str -> void
    ;; clears the current message text and replaces it with the given string
    (define set-txt
      (lambda (text-ed txt)
        (let ((text-obj (editor-text text-ed)))
          (send text-obj erase)
          (send text-obj insert txt))))

    ;; update : str -> void
    ;; gives the GUI a new message for its single window
    (define update
      (lambda (str)
        (send (editor-text one-message-ed) insert (format "~a~n" str))))

    ;; set-text : str -> void
    ;; sets the text in the message window to a particular value
    (define set-text
      (lambda (txt)
        (set-txt one-message-ed txt)))
    
    ;; get-thread : -> thread
    ;; returns the gui's thread ID
    (define get-thread
      (lambda () (current-thread)))
    
    ;; update-status : str -> void
    ;; updates the status bar with the given message
    (define update-status
      (lambda (message) (send top-frame set-status-text message)))
    
    ;; set-gauge-size : positive-natnum -> void
    ;; sets the minor gauge's size
    ;; OBTAINS LOCK lock
    (define set-gauge-size
      (lambda (size)
        (with-semaphore lock
                        (lambda ()
                          (do-reset-gauge)
                          (send gauge-minor set-range size)))))
    
    ;; set-overall-size : positive-natnum -> void
    ;; sets the overall gauge's size
    ;; OBTAINS LOCK lock
    (define set-overall-size
      (lambda (size)
        (with-semaphore lock
                        (lambda ()
                          (send gauge-major set-range size)))))
    
    ;; reset-overall-gauge : -> void
    ;; resets the overall gauge's count (but not its size)
    ;; OBTAINS LOCK lock
    (define reset-overall-gauge
      (lambda ()
        (with-semaphore lock
                        (lambda ()
                          (send gauge-major set-value 0)))))
    
    ;; reset-gauge : -> void
    ;; resets the minor gauge's count (but not its size)
    ;; OBTAINS LOCK lock
    (define reset-gauge
      (lambda ()
        (with-semaphore lock do-reset-gauge)))
    
    ;; do-reset-gauge : -> void
    ;; unlocked version of reset-gauge for internal use
    (define do-reset-gauge
      (lambda ()
        (send gauge-minor set-value 0)))
    
    ;; tick-gauge : -> void
    ;; ticks the major and minor gauges
    ;; OBTAINS LOCK lock
    (define tick-gauge
      (lambda ()
        (with-semaphore lock
           (lambda ()
             (send gauge-minor tick)
             (send gauge-major tick)))))

    ;; init : -> void
    ;; initializes the window
    (define init
      (lambda ()
        (send top-frame create-status-line)
        (send button-pane stretchable-height #f)
        (send button-pane set-alignment 'center 'center)
        (append-editor-operation-menu-items m-edit)
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