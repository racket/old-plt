(define standard-menus<%>
  (interface
    (basic<%>)
    get-menu%
    get-menu-item%
    get-checkable-menu-item%
    get-file-menu
    get-edit-menu
    get-help-menu
    file-menu:new
    file-menu:get-new-item
    file-menu:new-string
    file-menu:new-help-string
    file-menu:new-on-demand
    file-menu:between-new-and-open
    file-menu:open
    file-menu:get-open-item
    file-menu:open-string
    file-menu:open-help-string
    file-menu:open-on-demand
    file-menu:between-open-and-revert
    file-menu:revert
    file-menu:get-revert-item
    file-menu:revert-string
    file-menu:revert-help-string
    file-menu:revert-on-demand
    file-menu:between-revert-and-save
    file-menu:save
    file-menu:get-save-item
    file-menu:save-string
    file-menu:save-help-string
    file-menu:save-on-demand
    file-menu:save-as
    file-menu:get-save-as-item
    file-menu:save-as-string
    file-menu:save-as-help-string
    file-menu:save-as-on-demand
    file-menu:between-save-as-and-print
    file-menu:print
    file-menu:get-print-item
    file-menu:print-string
    file-menu:print-help-string
    file-menu:print-on-demand
    file-menu:between-print-and-close
    file-menu:close
    file-menu:get-close-item
    file-menu:close-string
    file-menu:close-help-string
    file-menu:close-on-demand
    file-menu:between-close-and-quit
    file-menu:quit
    file-menu:get-quit-item
    file-menu:quit-string
    file-menu:quit-help-string
    file-menu:quit-on-demand
    file-menu:after-quit
    edit-menu:undo
    edit-menu:get-undo-item
    edit-menu:undo-string
    edit-menu:undo-help-string
    edit-menu:undo-on-demand
    edit-menu:redo
    edit-menu:get-redo-item
    edit-menu:redo-string
    edit-menu:redo-help-string
    edit-menu:redo-on-demand
    edit-menu:between-redo-and-cut
    edit-menu:cut
    edit-menu:get-cut-item
    edit-menu:cut-string
    edit-menu:cut-help-string
    edit-menu:cut-on-demand
    edit-menu:between-cut-and-copy
    edit-menu:copy
    edit-menu:get-copy-item
    edit-menu:copy-string
    edit-menu:copy-help-string
    edit-menu:copy-on-demand
    edit-menu:between-copy-and-paste
    edit-menu:paste
    edit-menu:get-paste-item
    edit-menu:paste-string
    edit-menu:paste-help-string
    edit-menu:paste-on-demand
    edit-menu:between-paste-and-clear
    edit-menu:clear
    edit-menu:get-clear-item
    edit-menu:clear-string
    edit-menu:clear-help-string
    edit-menu:clear-on-demand
    edit-menu:between-clear-and-select-all
    edit-menu:select-all
    edit-menu:get-select-all-item
    edit-menu:select-all-string
    edit-menu:select-all-help-string
    edit-menu:select-all-on-demand
    edit-menu:between-select-all-and-find
    edit-menu:find
    edit-menu:get-find-item
    edit-menu:find-string
    edit-menu:find-help-string
    edit-menu:find-on-demand
    edit-menu:find-again
    edit-menu:get-find-again-item
    edit-menu:find-again-string
    edit-menu:find-again-help-string
    edit-menu:find-again-on-demand
    edit-menu:replace-and-find-again
    edit-menu:get-replace-and-find-again-item
    edit-menu:replace-and-find-again-string
    edit-menu:replace-and-find-again-help-string
    edit-menu:replace-and-find-again-on-demand
    edit-menu:between-find-and-preferences
    edit-menu:preferences
    edit-menu:get-preferences-item
    edit-menu:preferences-string
    edit-menu:preferences-help-string
    edit-menu:preferences-on-demand
    edit-menu:after-preferences
    help-menu:before-about
    help-menu:about
    help-menu:get-about-item
    help-menu:about-string
    help-menu:about-help-string
    help-menu:about-on-demand
    help-menu:after-about))

(define standard-menus-mixin
  (mixin
   (basic<%>)
   (standard-menus<%>)
   args
   (inherit on-menu-char on-traverse-char)
   (rename (super-on-close on-close))
   (private
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
   (override (on-close (lambda () (remove-prefs-callback) (super-on-close))))
   (inherit get-menu-bar show can-close? get-edit-target-object)
   (sequence (apply super-init args))
   (public (get-menu% (lambda () menu%)))
   (public (get-menu-item% (lambda () menu:can-restore-menu-item%)))
   (public
    (get-checkable-menu-item%
     (lambda () menu:can-restore-checkable-menu-item%)))
   (public
    (get-file-menu
     (let ((m
            (make-object (get-menu%)
              (if (eq? (system-type) 'windows) "&File" "F&ile")
              (get-menu-bar))))
       (lambda () m))))
   (public
    (get-edit-menu
     (let ((m (make-object (get-menu%) "&Edit" (get-menu-bar))))
       (lambda () m))))
   (public
    (get-help-menu
     (let ((m (make-object (get-menu%) "&Help" (get-menu-bar))))
       (lambda () m))))
   (public
    (file-menu:new (lambda (item control) (handler:edit-file #f) #t))
    (file-menu:get-new-item (lambda () file-menu:new-item))
    (file-menu:new-string (lambda () ""))
    (file-menu:new-help-string (lambda () "Open a new file"))
    (file-menu:new-on-demand void))
   (public (file-menu:between-new-and-open (lambda (menu) (void))))
   (public
    (file-menu:open (lambda (item control) (handler:open-file) #t))
    (file-menu:get-open-item (lambda () file-menu:open-item))
    (file-menu:open-string (lambda () ""))
    (file-menu:open-help-string (lambda () "Open a file from disk"))
    (file-menu:open-on-demand void))
   (public (file-menu:between-open-and-revert (lambda (menu) (void))))
   (public
    (file-menu:revert #f)
    (file-menu:get-revert-item (lambda () file-menu:revert-item))
    (file-menu:revert-string (lambda () ""))
    (file-menu:revert-help-string
     (lambda () "Revert this file to the copy on disk"))
    (file-menu:revert-on-demand void))
   (public (file-menu:between-revert-and-save (lambda (menu) (void))))
   (public
    (file-menu:save #f)
    (file-menu:get-save-item (lambda () file-menu:save-item))
    (file-menu:save-string (lambda () ""))
    (file-menu:save-help-string (lambda () "Save this file to disk"))
    (file-menu:save-on-demand void))
   (public
    (file-menu:save-as #f)
    (file-menu:get-save-as-item (lambda () file-menu:save-as-item))
    (file-menu:save-as-string (lambda () ""))
    (file-menu:save-as-help-string
     (lambda () "Prompt for a filename and save this file to disk"))
    (file-menu:save-as-on-demand void))
   (public
    (file-menu:between-save-as-and-print
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:print #f)
    (file-menu:get-print-item (lambda () file-menu:print-item))
    (file-menu:print-string (lambda () ""))
    (file-menu:print-help-string (lambda () "Print this file"))
    (file-menu:print-on-demand void))
   (public
    (file-menu:between-print-and-close
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:close
     (lambda (item control) (when (can-close?) (on-close) (show #f)) #t))
    (file-menu:get-close-item (lambda () file-menu:close-item))
    (file-menu:close-string (lambda () ""))
    (file-menu:close-help-string (lambda () "Close this file"))
    (file-menu:close-on-demand void))
   (public (file-menu:between-close-and-quit (lambda (menu) (void))))
   (public
    (file-menu:quit
     (lambda (item control)
       (parameterize ((exit:frame-exiting this)) (exit:exit))))
    (file-menu:get-quit-item (lambda () file-menu:quit-item))
    (file-menu:quit-string (lambda () ""))
    (file-menu:quit-help-string (lambda () "Quit"))
    (file-menu:quit-on-demand void))
   (public (file-menu:after-quit (lambda (menu) (void))))
   (public
    (edit-menu:undo
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
         (send item enable enable?)))))
   (public
    (edit-menu:redo
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
         (send item enable enable?)))))
   (public
    (edit-menu:between-redo-and-cut
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:cut
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
         (send item enable enable?)))))
   (public (edit-menu:between-cut-and-copy (lambda (menu) (void))))
   (public
    (edit-menu:copy
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
         (send item enable enable?)))))
   (public (edit-menu:between-copy-and-paste (lambda (menu) (void))))
   (public
    (edit-menu:paste
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
         (send item enable enable?)))))
   (public (edit-menu:between-paste-and-clear (lambda (menu) (void))))
   (public
    (edit-menu:clear
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
         (send item enable enable?)))))
   (public (edit-menu:between-clear-and-select-all (lambda (menu) (void))))
   (public
    (edit-menu:select-all
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
         (send item enable enable?)))))
   (public
    (edit-menu:between-select-all-and-find
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:find #f)
    (edit-menu:get-find-item (lambda () edit-menu:find-item))
    (edit-menu:find-string (lambda () ""))
    (edit-menu:find-help-string
     (lambda () "Search for a string in the window"))
    (edit-menu:find-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>)))))))
   (public
    (edit-menu:find-again #f)
    (edit-menu:get-find-again-item (lambda () edit-menu:find-again-item))
    (edit-menu:find-again-string (lambda () ""))
    (edit-menu:find-again-help-string
     (lambda () "Search for the same string as before"))
    (edit-menu:find-again-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>)))))))
   (public
    (edit-menu:replace-and-find-again #f)
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
          (and target (is-a? target editor<%>)))))))
   (public
    (edit-menu:between-find-and-preferences
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:preferences
     (lambda (item control) (preferences:show-dialog) #t))
    (edit-menu:get-preferences-item (lambda () edit-menu:preferences-item))
    (edit-menu:preferences-string (lambda () ""))
    (edit-menu:preferences-help-string (lambda () "Configure the preferences"))
    (edit-menu:preferences-on-demand void))
   (public (edit-menu:after-preferences (lambda (menu) (void))))
   (public (help-menu:before-about (lambda (menu) (void))))
   (public
    (help-menu:about #f)
    (help-menu:get-about-item (lambda () help-menu:about-item))
    (help-menu:about-string (lambda () ""))
    (help-menu:about-help-string
     (lambda () "Learn something about this application"))
    (help-menu:about-on-demand void))
   (public (help-menu:after-about (lambda (menu) (void))))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (private
    (file-menu:new-item
     (and file-menu:new
          (make-object (class
             (get-menu-item%)
             args
             (rename (super-on-demand on-demand))
             (override
               (on-demand
                 (lambda () (file-menu:new-on-demand this) (super-on-demand))))
             (sequence (apply super-init args)))
            (let
             ((special (file-menu:new-string)) (base "&New") (suffix ""))
             (if (string=? special "")
               (string-append base suffix)
               (string-append base " " special suffix)))
            (get-file-menu)
            (let
             ((file-menu:new (lambda (item evt) (file-menu:new item evt))))
             file-menu:new)
            #\n
            (file-menu:new-help-string)))))
   (sequence (file-menu:between-new-and-open (get-file-menu)))
   (private
    (file-menu:open-item
     (and file-menu:open
          (make-object (class
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
             ((special (file-menu:open-string)) (base "&Open") (suffix "..."))
             (if (string=? special "")
               (string-append base suffix)
               (string-append base " " special suffix)))
            (get-file-menu)
            (let
             ((file-menu:open (lambda (item evt) (file-menu:open item evt))))
             file-menu:open)
            #\o
            (file-menu:open-help-string)))))
   (sequence (file-menu:between-open-and-revert (get-file-menu)))
   (private
    (file-menu:revert-item
     (and file-menu:revert
          (make-object (class
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
             ((special (file-menu:revert-string)) (base "&Revert") (suffix ""))
             (if (string=? special "")
               (string-append base suffix)
               (string-append base " " special suffix)))
            (get-file-menu)
            (let
             ((file-menu:revert
                (lambda (item evt) (file-menu:revert item evt))))
             file-menu:revert)
            #f
            (file-menu:revert-help-string)))))
   (sequence (file-menu:between-revert-and-save (get-file-menu)))
   (private
    (file-menu:save-item
     (and file-menu:save
          (make-object (class
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
            (get-file-menu)
            (let
             ((file-menu:save (lambda (item evt) (file-menu:save item evt))))
             file-menu:save)
            #\s
            (file-menu:save-help-string)))))
   (private
    (file-menu:save-as-item
     (and file-menu:save-as
          (make-object (class
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
            (get-file-menu)
            (let
             ((file-menu:save-as
                (lambda (item evt) (file-menu:save-as item evt))))
             file-menu:save-as)
            #f
            (file-menu:save-as-help-string)))))
   (sequence (file-menu:between-save-as-and-print (get-file-menu)))
   (private
    (file-menu:print-item
     (and file-menu:print
          (make-object (class
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
            (get-file-menu)
            (let
             ((file-menu:print (lambda (item evt) (file-menu:print item evt))))
             file-menu:print)
            #\p
            (file-menu:print-help-string)))))
   (sequence (file-menu:between-print-and-close (get-file-menu)))
   (private
    (file-menu:close-item
     (and file-menu:close
          (make-object (class
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
            (get-file-menu)
            (let
             ((file-menu:close (lambda (item evt) (file-menu:close item evt))))
             file-menu:close)
            #\w
            (file-menu:close-help-string)))))
   (sequence (file-menu:between-close-and-quit (get-file-menu)))
   (private
    (file-menu:quit-item
     (and file-menu:quit
          (make-object (class
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
            (get-file-menu)
            (let
             ((file-menu:quit (lambda (item evt) (file-menu:quit item evt))))
             file-menu:quit)
            #\q
            (file-menu:quit-help-string)))))
   (sequence (file-menu:after-quit (get-file-menu)))
   (private
    (edit-menu:undo-item
     (and edit-menu:undo
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:undo (lambda (item evt) (edit-menu:undo item evt))))
             edit-menu:undo)
            #\z
            (edit-menu:undo-help-string)))))
   (private
    (edit-menu:redo-item
     (and edit-menu:redo
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:redo (lambda (item evt) (edit-menu:redo item evt))))
             edit-menu:redo)
            #\y
            (edit-menu:redo-help-string)))))
   (sequence (edit-menu:between-redo-and-cut (get-edit-menu)))
   (private
    (edit-menu:cut-item
     (and edit-menu:cut
          (make-object (class
             (get-menu-item%)
             args
             (rename (super-on-demand on-demand))
             (override
               (on-demand
                 (lambda () (edit-menu:cut-on-demand this) (super-on-demand))))
             (sequence (apply super-init args)))
            (let
             ((special (edit-menu:cut-string)) (base "Cu&t") (suffix ""))
             (if (string=? special "")
               (string-append base suffix)
               (string-append base " " special suffix)))
            (get-edit-menu)
            (let
             ((edit-menu:cut (lambda (item evt) (edit-menu:cut item evt))))
             edit-menu:cut)
            #\x
            (edit-menu:cut-help-string)))))
   (sequence (edit-menu:between-cut-and-copy (get-edit-menu)))
   (private
    (edit-menu:copy-item
     (and edit-menu:copy
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:copy (lambda (item evt) (edit-menu:copy item evt))))
             edit-menu:copy)
            #\c
            (edit-menu:copy-help-string)))))
   (sequence (edit-menu:between-copy-and-paste (get-edit-menu)))
   (private
    (edit-menu:paste-item
     (and edit-menu:paste
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:paste (lambda (item evt) (edit-menu:paste item evt))))
             edit-menu:paste)
            #\v
            (edit-menu:paste-help-string)))))
   (sequence (edit-menu:between-paste-and-clear (get-edit-menu)))
   (private
    (edit-menu:clear-item
     (and edit-menu:clear
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:clear (lambda (item evt) (edit-menu:clear item evt))))
             edit-menu:clear)
            #f
            (edit-menu:clear-help-string)))))
   (sequence (edit-menu:between-clear-and-select-all (get-edit-menu)))
   (private
    (edit-menu:select-all-item
     (and edit-menu:select-all
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:select-all
                (lambda (item evt) (edit-menu:select-all item evt))))
             edit-menu:select-all)
            #\a
            (edit-menu:select-all-help-string)))))
   (sequence (edit-menu:between-select-all-and-find (get-edit-menu)))
   (private
    (edit-menu:find-item
     (and edit-menu:find
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:find (lambda (item evt) (edit-menu:find item evt))))
             edit-menu:find)
            #\f
            (edit-menu:find-help-string)))))
   (private
    (edit-menu:find-again-item
     (and edit-menu:find-again
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:find-again
                (lambda (item evt) (edit-menu:find-again item evt))))
             edit-menu:find-again)
            #\g
            (edit-menu:find-again-help-string)))))
   (private
    (edit-menu:replace-and-find-again-item
     (and edit-menu:replace-and-find-again
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:replace-and-find-again
                (lambda (item evt)
                  (edit-menu:replace-and-find-again item evt))))
             edit-menu:replace-and-find-again)
            #\h
            (edit-menu:replace-and-find-again-help-string)))))
   (sequence (edit-menu:between-find-and-preferences (get-edit-menu)))
   (private
    (edit-menu:preferences-item
     (and edit-menu:preferences
          (make-object (class
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
            (get-edit-menu)
            (let
             ((edit-menu:preferences
                (lambda (item evt) (edit-menu:preferences item evt))))
             edit-menu:preferences)
            #f
            (edit-menu:preferences-help-string)))))
   (sequence (edit-menu:after-preferences (get-edit-menu)))
   (sequence (help-menu:before-about (get-help-menu)))
   (private
    (help-menu:about-item
     (and help-menu:about
          (make-object (class
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
            (get-help-menu)
            (let
             ((help-menu:about (lambda (item evt) (help-menu:about item evt))))
             help-menu:about)
            #f
            (help-menu:about-help-string)))))
   (sequence (help-menu:after-about (get-help-menu)))
   (sequence (reorder-menus this))))
