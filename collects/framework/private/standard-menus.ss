(define standard-menus<%>
  (interface
    (basic<%>)
    get-menu%
    get-menu-item%
    get-checkable-menu-item%
    get-file-menu
    get-edit-menu
    get-help-menu
    file-menu:new-callback
    file-menu:get-new-item
    file-menu:new-string
    file-menu:new-help-string
    file-menu:new-on-demand
    file-menu:create-new?
    file-menu:between-new-and-open
    file-menu:open-callback
    file-menu:get-open-item
    file-menu:open-string
    file-menu:open-help-string
    file-menu:open-on-demand
    file-menu:create-open?
    file-menu:between-open-and-revert
    file-menu:revert-callback
    file-menu:get-revert-item
    file-menu:revert-string
    file-menu:revert-help-string
    file-menu:revert-on-demand
    file-menu:create-revert?
    file-menu:between-revert-and-save
    file-menu:save-callback
    file-menu:get-save-item
    file-menu:save-string
    file-menu:save-help-string
    file-menu:save-on-demand
    file-menu:create-save?
    file-menu:save-as-callback
    file-menu:get-save-as-item
    file-menu:save-as-string
    file-menu:save-as-help-string
    file-menu:save-as-on-demand
    file-menu:create-save-as?
    file-menu:between-save-as-and-print
    file-menu:print-callback
    file-menu:get-print-item
    file-menu:print-string
    file-menu:print-help-string
    file-menu:print-on-demand
    file-menu:create-print?
    file-menu:between-print-and-close
    file-menu:close-callback
    file-menu:get-close-item
    file-menu:close-string
    file-menu:close-help-string
    file-menu:close-on-demand
    file-menu:create-close?
    file-menu:between-close-and-quit
    file-menu:quit-callback
    file-menu:get-quit-item
    file-menu:quit-string
    file-menu:quit-help-string
    file-menu:quit-on-demand
    file-menu:create-quit?
    file-menu:after-quit
    edit-menu:undo-callback
    edit-menu:get-undo-item
    edit-menu:undo-string
    edit-menu:undo-help-string
    edit-menu:undo-on-demand
    edit-menu:create-undo?
    edit-menu:redo-callback
    edit-menu:get-redo-item
    edit-menu:redo-string
    edit-menu:redo-help-string
    edit-menu:redo-on-demand
    edit-menu:create-redo?
    edit-menu:between-redo-and-cut
    edit-menu:cut-callback
    edit-menu:get-cut-item
    edit-menu:cut-string
    edit-menu:cut-help-string
    edit-menu:cut-on-demand
    edit-menu:create-cut?
    edit-menu:between-cut-and-copy
    edit-menu:copy-callback
    edit-menu:get-copy-item
    edit-menu:copy-string
    edit-menu:copy-help-string
    edit-menu:copy-on-demand
    edit-menu:create-copy?
    edit-menu:between-copy-and-paste
    edit-menu:paste-callback
    edit-menu:get-paste-item
    edit-menu:paste-string
    edit-menu:paste-help-string
    edit-menu:paste-on-demand
    edit-menu:create-paste?
    edit-menu:between-paste-and-clear
    edit-menu:clear-callback
    edit-menu:get-clear-item
    edit-menu:clear-string
    edit-menu:clear-help-string
    edit-menu:clear-on-demand
    edit-menu:create-clear?
    edit-menu:between-clear-and-select-all
    edit-menu:select-all-callback
    edit-menu:get-select-all-item
    edit-menu:select-all-string
    edit-menu:select-all-help-string
    edit-menu:select-all-on-demand
    edit-menu:create-select-all?
    edit-menu:between-select-all-and-find
    edit-menu:find-callback
    edit-menu:get-find-item
    edit-menu:find-string
    edit-menu:find-help-string
    edit-menu:find-on-demand
    edit-menu:create-find?
    edit-menu:find-again-callback
    edit-menu:get-find-again-item
    edit-menu:find-again-string
    edit-menu:find-again-help-string
    edit-menu:find-again-on-demand
    edit-menu:create-find-again?
    edit-menu:replace-and-find-again-callback
    edit-menu:get-replace-and-find-again-item
    edit-menu:replace-and-find-again-string
    edit-menu:replace-and-find-again-help-string
    edit-menu:replace-and-find-again-on-demand
    edit-menu:create-replace-and-find-again?
    edit-menu:between-find-and-preferences
    edit-menu:preferences-callback
    edit-menu:get-preferences-item
    edit-menu:preferences-string
    edit-menu:preferences-help-string
    edit-menu:preferences-on-demand
    edit-menu:create-preferences?
    edit-menu:after-preferences
    help-menu:before-about
    help-menu:about-callback
    help-menu:get-about-item
    help-menu:about-string
    help-menu:about-help-string
    help-menu:about-on-demand
    help-menu:create-about?
    help-menu:after-about))

(define standard-menus-mixin
  (mixin
   (basic<%>)
   (standard-menus<%>)
   args
   (inherit on-menu-char on-traverse-char)
   (private-field
     (remove-prefs-callback
       (preferences:add-callback
         'framework:menu-bindings
         (lambda (p v)
           (let ((mb (get-menu-bar)))
             (let loop ((menu (get-menu-bar)))
               (cond
                ((is-a? menu menu-item-container<%>)
                 (for-each loop (send menu get-items)))
                ((is-a? menu selectable-menu-item<%>)
                 (when (is-a? menu menu:can-restore<%>)
                   (if v
                     (send menu restore-keybinding)
                     (send menu set-shortcut #f)))))))))))
   (inherit get-menu-bar show can-close? get-edit-target-object)
   (override (on-close (lambda () (remove-prefs-callback) (super-on-close))))
   (public (get-menu% (lambda () menu%)))
   (public (get-menu-item% (lambda () menu:can-restore-menu-item%)))
   (public
    (get-checkable-menu-item%
     (lambda () menu:can-restore-checkable-menu-item%)))
   (public (get-file-menu (lambda () file-menu)))
   (sequence (void))
   (public (get-edit-menu (lambda () edit-menu)))
   (sequence (void))
   (public (get-help-menu (lambda () help-menu)))
   (sequence (void))
   (public
    (file-menu:new-callback (lambda (item control) (handler:edit-file #f) #t))
    (file-menu:get-new-item (lambda () file-menu:new-item))
    (file-menu:new-string (lambda () ""))
    (file-menu:new-help-string (lambda () "Open a new file"))
    (file-menu:new-on-demand (lambda () (void)))
    (file-menu:create-new? (lambda () #t)))
   (public (file-menu:between-new-and-open (lambda (menu) (void))))
   (public
    (file-menu:open-callback (lambda (item control) (handler:open-file) #t))
    (file-menu:get-open-item (lambda () file-menu:open-item))
    (file-menu:open-string (lambda () ""))
    (file-menu:open-help-string (lambda () "Open a file from disk"))
    (file-menu:open-on-demand (lambda () (void)))
    (file-menu:create-open? (lambda () #t)))
   (public (file-menu:between-open-and-revert (lambda (menu) (void))))
   (public
    (file-menu:revert-callback (lambda (x y) (void)))
    (file-menu:get-revert-item (lambda () file-menu:revert-item))
    (file-menu:revert-string (lambda () ""))
    (file-menu:revert-help-string
     (lambda () "Revert this file to the copy on disk"))
    (file-menu:revert-on-demand (lambda () (void)))
    (file-menu:create-revert? (lambda () #f)))
   (public (file-menu:between-revert-and-save (lambda (menu) (void))))
   (public
    (file-menu:save-callback (lambda (x y) (void)))
    (file-menu:get-save-item (lambda () file-menu:save-item))
    (file-menu:save-string (lambda () ""))
    (file-menu:save-help-string (lambda () "Save this file to disk"))
    (file-menu:save-on-demand (lambda () (void)))
    (file-menu:create-save? (lambda () #f)))
   (public
    (file-menu:save-as-callback (lambda (x y) (void)))
    (file-menu:get-save-as-item (lambda () file-menu:save-as-item))
    (file-menu:save-as-string (lambda () ""))
    (file-menu:save-as-help-string
     (lambda () "Prompt for a filename and save this file to disk"))
    (file-menu:save-as-on-demand (lambda () (void)))
    (file-menu:create-save-as? (lambda () #f)))
   (public
    (file-menu:between-save-as-and-print
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:print-callback (lambda (x y) (void)))
    (file-menu:get-print-item (lambda () file-menu:print-item))
    (file-menu:print-string (lambda () ""))
    (file-menu:print-help-string (lambda () "Print this file"))
    (file-menu:print-on-demand (lambda () (void)))
    (file-menu:create-print? (lambda () #f)))
   (public
    (file-menu:between-print-and-close
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:close-callback
     (lambda (item control) (when (can-close?) (on-close) (show #f)) #t))
    (file-menu:get-close-item (lambda () file-menu:close-item))
    (file-menu:close-string (lambda () ""))
    (file-menu:close-help-string (lambda () "Close this file"))
    (file-menu:close-on-demand (lambda () (void)))
    (file-menu:create-close? (lambda () #t)))
   (public (file-menu:between-close-and-quit (lambda (menu) (void))))
   (public
    (file-menu:quit-callback
     (lambda (item control)
       (parameterize ((exit:frame-exiting this)) (exit:exit))))
    (file-menu:get-quit-item (lambda () file-menu:quit-item))
    (file-menu:quit-string (lambda () ""))
    (file-menu:quit-help-string (lambda () "Quit"))
    (file-menu:quit-on-demand (lambda () (void)))
    (file-menu:create-quit? (lambda () #t)))
   (public (file-menu:after-quit (lambda (menu) (void))))
   (public
    (edit-menu:undo-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'undo)))
       #t))
    (edit-menu:get-undo-item (lambda () edit-menu:undo-item))
    (edit-menu:undo-string (lambda () ""))
    (edit-menu:undo-help-string (lambda () "Undo the most recent action"))
    (edit-menu:undo-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'undo))))
         (send item enable enable?))))
    (edit-menu:create-undo? (lambda () #t)))
   (public
    (edit-menu:redo-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'redo)))
       #t))
    (edit-menu:get-redo-item (lambda () edit-menu:redo-item))
    (edit-menu:redo-string (lambda () ""))
    (edit-menu:redo-help-string (lambda () "Redo the most recent undo"))
    (edit-menu:redo-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'redo))))
         (send item enable enable?))))
    (edit-menu:create-redo? (lambda () #t)))
   (public
    (edit-menu:between-redo-and-cut
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:cut-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'cut)))
       #t))
    (edit-menu:get-cut-item (lambda () edit-menu:cut-item))
    (edit-menu:cut-string (lambda () ""))
    (edit-menu:cut-help-string (lambda () "Cut the selection"))
    (edit-menu:cut-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'cut))))
         (send item enable enable?))))
    (edit-menu:create-cut? (lambda () #t)))
   (public (edit-menu:between-cut-and-copy (lambda (menu) (void))))
   (public
    (edit-menu:copy-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'copy)))
       #t))
    (edit-menu:get-copy-item (lambda () edit-menu:copy-item))
    (edit-menu:copy-string (lambda () ""))
    (edit-menu:copy-help-string (lambda () "Copy the selection"))
    (edit-menu:copy-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'copy))))
         (send item enable enable?))))
    (edit-menu:create-copy? (lambda () #t)))
   (public (edit-menu:between-copy-and-paste (lambda (menu) (void))))
   (public
    (edit-menu:paste-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'paste)))
       #t))
    (edit-menu:get-paste-item (lambda () edit-menu:paste-item))
    (edit-menu:paste-string (lambda () ""))
    (edit-menu:paste-help-string
     (lambda () "Paste the most recent copy or cut over the selection"))
    (edit-menu:paste-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'paste))))
         (send item enable enable?))))
    (edit-menu:create-paste? (lambda () #t)))
   (public (edit-menu:between-paste-and-clear (lambda (menu) (void))))
   (public
    (edit-menu:clear-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'clear)))
       #t))
    (edit-menu:get-clear-item (lambda () edit-menu:clear-item))
    (edit-menu:clear-string (lambda () ""))
    (edit-menu:clear-help-string
     (lambda () "Clear the selection without affecting paste"))
    (edit-menu:clear-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'clear))))
         (send item enable enable?))))
    (edit-menu:create-clear? (lambda () #t)))
   (public (edit-menu:between-clear-and-select-all (lambda (menu) (void))))
   (public
    (edit-menu:select-all-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'select-all)))
       #t))
    (edit-menu:get-select-all-item (lambda () edit-menu:select-all-item))
    (edit-menu:select-all-string (lambda () ""))
    (edit-menu:select-all-help-string (lambda () "Select the entire document"))
    (edit-menu:select-all-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'select-all))))
         (send item enable enable?))))
    (edit-menu:create-select-all? (lambda () #t)))
   (public
    (edit-menu:between-select-all-and-find
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:find-callback (lambda (x y) (void)))
    (edit-menu:get-find-item (lambda () edit-menu:find-item))
    (edit-menu:find-string (lambda () ""))
    (edit-menu:find-help-string
     (lambda () "Search for a string in the window"))
    (edit-menu:find-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
    (edit-menu:create-find? (lambda () #f)))
   (public
    (edit-menu:find-again-callback (lambda (x y) (void)))
    (edit-menu:get-find-again-item (lambda () edit-menu:find-again-item))
    (edit-menu:find-again-string (lambda () ""))
    (edit-menu:find-again-help-string
     (lambda () "Search for the same string as before"))
    (edit-menu:find-again-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
    (edit-menu:create-find-again? (lambda () #f)))
   (public
    (edit-menu:replace-and-find-again-callback (lambda (x y) (void)))
    (edit-menu:get-replace-and-find-again-item
     (lambda () edit-menu:replace-and-find-again-item))
    (edit-menu:replace-and-find-again-string (lambda () ""))
    (edit-menu:replace-and-find-again-help-string
     (lambda ()
       "Replace the current text and search for the same string as before"))
    (edit-menu:replace-and-find-again-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
    (edit-menu:create-replace-and-find-again? (lambda () #f)))
   (public
    (edit-menu:between-find-and-preferences
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:preferences-callback
     (lambda (item control) (preferences:show-dialog) #t))
    (edit-menu:get-preferences-item (lambda () edit-menu:preferences-item))
    (edit-menu:preferences-string (lambda () ""))
    (edit-menu:preferences-help-string (lambda () "Configure the preferences"))
    (edit-menu:preferences-on-demand (lambda () (void)))
    (edit-menu:create-preferences? (lambda () #t)))
   (public (edit-menu:after-preferences (lambda (menu) (void))))
   (public (help-menu:before-about (lambda (menu) (void))))
   (public
    (help-menu:about-callback (lambda (x y) (void)))
    (help-menu:get-about-item (lambda () help-menu:about-item))
    (help-menu:about-string (lambda () ""))
    (help-menu:about-help-string
     (lambda () "Learn something about this application"))
    (help-menu:about-on-demand (lambda () (void)))
    (help-menu:create-about? (lambda () #f)))
   (public (help-menu:after-about (lambda (menu) (void))))
   (sequence (apply super-init args))
   (rename (super-on-close on-close))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (private-field
     (file-menu
       (make-object (get-menu%)
         (if (eq? (system-type) 'windows) "&File" "F&ile")
         (get-menu-bar))))
   (sequence (void))
   (private-field (edit-menu (make-object (get-menu%) "&Edit" (get-menu-bar))))
   (sequence (void))
   (private-field (help-menu (make-object (get-menu%) "&Help" (get-menu-bar))))
   (private-field
     (file-menu:new-item
       (and (file-menu:create-new?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:new-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:new-string)) (base "&New") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:new-callback
                  (lambda (item evt) (file-menu:new-callback item evt))))
               file-menu:new-callback)
              #\n
              (file-menu:new-help-string)))))
   (sequence (file-menu:between-new-and-open (get-file-menu)))
   (private-field
     (file-menu:open-item
       (and (file-menu:create-open?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:open-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:open-string))
                (base "&Open")
                (suffix "..."))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:open-callback
                  (lambda (item evt) (file-menu:open-callback item evt))))
               file-menu:open-callback)
              #\o
              (file-menu:open-help-string)))))
   (sequence (file-menu:between-open-and-revert (get-file-menu)))
   (private-field
     (file-menu:revert-item
       (and (file-menu:create-revert?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:revert-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:revert-string))
                (base "&Revert")
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:revert-callback
                  (lambda (item evt) (file-menu:revert-callback item evt))))
               file-menu:revert-callback)
              #f
              (file-menu:revert-help-string)))))
   (sequence (file-menu:between-revert-and-save (get-file-menu)))
   (private-field
     (file-menu:save-item
       (and (file-menu:create-save?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:save-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:save-string)) (base "&Save") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:save-callback
                  (lambda (item evt) (file-menu:save-callback item evt))))
               file-menu:save-callback)
              #\s
              (file-menu:save-help-string)))))
   (private-field
     (file-menu:save-as-item
       (and (file-menu:create-save-as?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:save-as-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:save-as-string))
                (base "Save")
                (suffix " &As..."))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:save-as-callback
                  (lambda (item evt) (file-menu:save-as-callback item evt))))
               file-menu:save-as-callback)
              #f
              (file-menu:save-as-help-string)))))
   (sequence (file-menu:between-save-as-and-print (get-file-menu)))
   (private-field
     (file-menu:print-item
       (and (file-menu:create-print?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:print-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:print-string))
                (base "&Print")
                (suffix "..."))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:print-callback
                  (lambda (item evt) (file-menu:print-callback item evt))))
               file-menu:print-callback)
              #\p
              (file-menu:print-help-string)))))
   (sequence (file-menu:between-print-and-close (get-file-menu)))
   (private-field
     (file-menu:close-item
       (and (file-menu:create-close?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:close-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:close-string)) (base "&Close") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:close-callback
                  (lambda (item evt) (file-menu:close-callback item evt))))
               file-menu:close-callback)
              #\w
              (file-menu:close-help-string)))))
   (sequence (file-menu:between-close-and-quit (get-file-menu)))
   (private-field
     (file-menu:quit-item
       (and (file-menu:create-quit?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (file-menu:quit-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (file-menu:quit-string))
                (base (if (eq? (system-type) 'windows) "E&xit" "Quit"))
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              file-menu
              (let
               ((file-menu:quit-callback
                  (lambda (item evt) (file-menu:quit-callback item evt))))
               file-menu:quit-callback)
              #\q
              (file-menu:quit-help-string)))))
   (sequence (file-menu:after-quit (get-file-menu)))
   (private-field
     (edit-menu:undo-item
       (and (edit-menu:create-undo?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:undo-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:undo-string)) (base "&Undo") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:undo-callback
                  (lambda (item evt) (edit-menu:undo-callback item evt))))
               edit-menu:undo-callback)
              #\z
              (edit-menu:undo-help-string)))))
   (private-field
     (edit-menu:redo-item
       (and (edit-menu:create-redo?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:redo-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:redo-string)) (base "&Redo") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:redo-callback
                  (lambda (item evt) (edit-menu:redo-callback item evt))))
               edit-menu:redo-callback)
              #\y
              (edit-menu:redo-help-string)))))
   (sequence (edit-menu:between-redo-and-cut (get-edit-menu)))
   (private-field
     (edit-menu:cut-item
       (and (edit-menu:create-cut?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:cut-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:cut-string)) (base "Cu&t") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:cut-callback
                  (lambda (item evt) (edit-menu:cut-callback item evt))))
               edit-menu:cut-callback)
              #\x
              (edit-menu:cut-help-string)))))
   (sequence (edit-menu:between-cut-and-copy (get-edit-menu)))
   (private-field
     (edit-menu:copy-item
       (and (edit-menu:create-copy?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:copy-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:copy-string)) (base "&Copy") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:copy-callback
                  (lambda (item evt) (edit-menu:copy-callback item evt))))
               edit-menu:copy-callback)
              #\c
              (edit-menu:copy-help-string)))))
   (sequence (edit-menu:between-copy-and-paste (get-edit-menu)))
   (private-field
     (edit-menu:paste-item
       (and (edit-menu:create-paste?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:paste-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:paste-string)) (base "&Paste") (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:paste-callback
                  (lambda (item evt) (edit-menu:paste-callback item evt))))
               edit-menu:paste-callback)
              #\v
              (edit-menu:paste-help-string)))))
   (sequence (edit-menu:between-paste-and-clear (get-edit-menu)))
   (private-field
     (edit-menu:clear-item
       (and (edit-menu:create-clear?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:clear-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:clear-string))
                (base (if (eq? (system-type) 'macos) "Clear" "&Delete"))
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:clear-callback
                  (lambda (item evt) (edit-menu:clear-callback item evt))))
               edit-menu:clear-callback)
              #f
              (edit-menu:clear-help-string)))))
   (sequence (edit-menu:between-clear-and-select-all (get-edit-menu)))
   (private-field
     (edit-menu:select-all-item
       (and (edit-menu:create-select-all?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:select-all-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:select-all-string))
                (base "Select A&ll")
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:select-all-callback
                  (lambda (item evt)
                    (edit-menu:select-all-callback item evt))))
               edit-menu:select-all-callback)
              #\a
              (edit-menu:select-all-help-string)))))
   (sequence (edit-menu:between-select-all-and-find (get-edit-menu)))
   (private-field
     (edit-menu:find-item
       (and (edit-menu:create-find?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:find-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:find-string)) (base "Find") (suffix "..."))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:find-callback
                  (lambda (item evt) (edit-menu:find-callback item evt))))
               edit-menu:find-callback)
              #\f
              (edit-menu:find-help-string)))))
   (private-field
     (edit-menu:find-again-item
       (and (edit-menu:create-find-again?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:find-again-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:find-again-string))
                (base "Find Again")
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:find-again-callback
                  (lambda (item evt)
                    (edit-menu:find-again-callback item evt))))
               edit-menu:find-again-callback)
              #\g
              (edit-menu:find-again-help-string)))))
   (private-field
     (edit-menu:replace-and-find-again-item
       (and (edit-menu:create-replace-and-find-again?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:replace-and-find-again-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:replace-and-find-again-string))
                (base "Replace && Find Again")
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:replace-and-find-again-callback
                  (lambda (item evt)
                    (edit-menu:replace-and-find-again-callback item evt))))
               edit-menu:replace-and-find-again-callback)
              #\h
              (edit-menu:replace-and-find-again-help-string)))))
   (sequence (edit-menu:between-find-and-preferences (get-edit-menu)))
   (private-field
     (edit-menu:preferences-item
       (and (edit-menu:create-preferences?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (edit-menu:preferences-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (edit-menu:preferences-string))
                (base "Preferences...")
                (suffix ""))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              edit-menu
              (let
               ((edit-menu:preferences-callback
                  (lambda (item evt)
                    (edit-menu:preferences-callback item evt))))
               edit-menu:preferences-callback)
              #f
              (edit-menu:preferences-help-string)))))
   (sequence (edit-menu:after-preferences (get-edit-menu)))
   (sequence (help-menu:before-about (get-help-menu)))
   (private-field
     (help-menu:about-item
       (and (help-menu:create-about?)
            (make-object (class100
               (get-menu-item%)
               args
               (rename (super-on-demand on-demand))
               (override
                 (on-demand
                   (lambda ()
                     (help-menu:about-on-demand this)
                     (super-on-demand))))
               (sequence (apply super-init args)))
              (let
               ((special (help-menu:about-string))
                (base "About ")
                (suffix "..."))
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
              help-menu
              (let
               ((help-menu:about-callback
                  (lambda (item evt) (help-menu:about-callback item evt))))
               help-menu:about-callback)
              #f
              (help-menu:about-help-string)))))
   (sequence (help-menu:after-about (get-help-menu)))
   (sequence (reorder-menus this))))
