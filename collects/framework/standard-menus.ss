(define standard-menus<%>
  (interface
    (basic<%>)
    get-menu%
    get-menu-item%
    get-file-menu
    get-edit-menu
    get-help-menu
    file-menu:new
    file-menu:get-new-item
    file-menu:new-string
    file-menu:new-help-string
    file-menu:between-new-and-open
    file-menu:open
    file-menu:get-open-item
    file-menu:open-string
    file-menu:open-help-string
    file-menu:between-open-and-revert
    file-menu:revert
    file-menu:get-revert-item
    file-menu:revert-string
    file-menu:revert-help-string
    file-menu:between-revert-and-save
    file-menu:save
    file-menu:get-save-item
    file-menu:save-string
    file-menu:save-help-string
    file-menu:save-as
    file-menu:get-save-as-item
    file-menu:save-as-string
    file-menu:save-as-help-string
    file-menu:between-save-as-and-print
    file-menu:print
    file-menu:get-print-item
    file-menu:print-string
    file-menu:print-help-string
    file-menu:between-print-and-close
    file-menu:close
    file-menu:get-close-item
    file-menu:close-string
    file-menu:close-help-string
    file-menu:between-close-and-quit
    file-menu:quit
    file-menu:get-quit-item
    file-menu:quit-string
    file-menu:quit-help-string
    file-menu:after-quit
    edit-menu:undo
    edit-menu:get-undo-item
    edit-menu:undo-string
    edit-menu:undo-help-string
    edit-menu:redo
    edit-menu:get-redo-item
    edit-menu:redo-string
    edit-menu:redo-help-string
    edit-menu:between-redo-and-cut
    edit-menu:cut
    edit-menu:get-cut-item
    edit-menu:cut-string
    edit-menu:cut-help-string
    edit-menu:between-cut-and-copy
    edit-menu:copy
    edit-menu:get-copy-item
    edit-menu:copy-string
    edit-menu:copy-help-string
    edit-menu:between-copy-and-paste
    edit-menu:paste
    edit-menu:get-paste-item
    edit-menu:paste-string
    edit-menu:paste-help-string
    edit-menu:between-paste-and-clear
    edit-menu:clear
    edit-menu:get-clear-item
    edit-menu:clear-string
    edit-menu:clear-help-string
    edit-menu:between-clear-and-select-all
    edit-menu:select-all
    edit-menu:get-select-all-item
    edit-menu:select-all-string
    edit-menu:select-all-help-string
    edit-menu:between-select-all-and-find
    edit-menu:find
    edit-menu:get-find-item
    edit-menu:find-string
    edit-menu:find-help-string
    edit-menu:between-find-and-preferences
    edit-menu:preferences
    edit-menu:get-preferences-item
    edit-menu:preferences-string
    edit-menu:preferences-help-string
    edit-menu:after-preferences
    help-menu:before-about
    help-menu:about
    help-menu:get-about-item
    help-menu:about-string
    help-menu:about-help-string
    help-menu:after-about))

(define standard-menus-mixin
  (mixin
   (basic<%>)
   (standard-menus<%>)
   args
   (inherit get-menu-bar can-close? on-close show get-edit-target-object)
   (sequence (apply super-init args))
   (public (get-menu% (lambda () menu%)))
   (public (get-menu-item% (lambda () menu-item%)))
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
    (file-menu:new-help-string (lambda () "Open a new file")))
   (public (file-menu:between-new-and-open (lambda (menu) (void))))
   (public
    (file-menu:open (lambda (item control) (handler:open-file) #t))
    (file-menu:get-open-item (lambda () file-menu:open-item))
    (file-menu:open-string (lambda () ""))
    (file-menu:open-help-string (lambda () "Open a file from disk")))
   (public (file-menu:between-open-and-revert (lambda (menu) (void))))
   (public
    (file-menu:revert #f)
    (file-menu:get-revert-item (lambda () file-menu:revert-item))
    (file-menu:revert-string (lambda () ""))
    (file-menu:revert-help-string
     (lambda () "Revert this file to the copy on disk")))
   (public (file-menu:between-revert-and-save (lambda (menu) (void))))
   (public
    (file-menu:save #f)
    (file-menu:get-save-item (lambda () file-menu:save-item))
    (file-menu:save-string (lambda () ""))
    (file-menu:save-help-string (lambda () "Save this file to disk")))
   (public
    (file-menu:save-as #f)
    (file-menu:get-save-as-item (lambda () file-menu:save-as-item))
    (file-menu:save-as-string (lambda () ""))
    (file-menu:save-as-help-string
     (lambda () "Prompt for a filename and save this file to disk")))
   (public
    (file-menu:between-save-as-and-print
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:print #f)
    (file-menu:get-print-item (lambda () file-menu:print-item))
    (file-menu:print-string (lambda () ""))
    (file-menu:print-help-string (lambda () "Print this file")))
   (public
    (file-menu:between-print-and-close
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (file-menu:close
     (lambda (item control) (when (can-close?) (on-close) (show #f)) #t))
    (file-menu:get-close-item (lambda () file-menu:close-item))
    (file-menu:close-string (lambda () ""))
    (file-menu:close-help-string (lambda () "Close this file")))
   (public (file-menu:between-close-and-quit (lambda (menu) (void))))
   (public
    (file-menu:quit (lambda (item control) (exit:exit)))
    (file-menu:get-quit-item (lambda () file-menu:quit-item))
    (file-menu:quit-string (lambda () ""))
    (file-menu:quit-help-string (lambda () "Quit")))
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
    (edit-menu:undo-help-string (lambda () "Undo the most recent action")))
   (public
    (edit-menu:redo
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'redo)))
       #t))
    (edit-menu:get-redo-item (lambda () edit-menu:redo-item))
    (edit-menu:redo-string (lambda () ""))
    (edit-menu:redo-help-string (lambda () "Redo the most recent undo")))
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
    (edit-menu:cut-help-string (lambda () "Cut the selection")))
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
    (edit-menu:copy-help-string (lambda () "Copy the selection")))
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
     (lambda () "Paste the most recent copy or cut over the selection")))
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
     (lambda () "Clear the selection without affecting paste")))
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
    (edit-menu:select-all-help-string
     (lambda () "Select the entire document")))
   (public
    (edit-menu:between-select-all-and-find
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:find #f)
    (edit-menu:get-find-item (lambda () edit-menu:find-item))
    (edit-menu:find-string (lambda () ""))
    (edit-menu:find-help-string
     (lambda () "Search for a string in the window")))
   (public
    (edit-menu:between-find-and-preferences
     (lambda (menu) (make-object separator-menu-item% menu))))
   (public
    (edit-menu:preferences
     (lambda (item control) (preferences:show-dialog) #t))
    (edit-menu:get-preferences-item (lambda () edit-menu:preferences-item))
    (edit-menu:preferences-string (lambda () ""))
    (edit-menu:preferences-help-string
     (lambda () "Configure the preferences")))
   (public (edit-menu:after-preferences (lambda (menu) (void))))
   (public (help-menu:before-about (lambda (menu) (void))))
   (public
    (help-menu:about #f)
    (help-menu:get-about-item (lambda () help-menu:about-item))
    (help-menu:about-string (lambda () ""))
    (help-menu:about-help-string
     (lambda () "Learn something about this application")))
   (public (help-menu:after-about (lambda (menu) (void))))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (sequence (void))
   (private
    (file-menu:new-item
     (and file-menu:new
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&New"
             (file-menu:new-string)
             "")
            (get-file-menu)
            file-menu:new
            (if (preferences:get 'framework:menu-bindings) #\n #f)
            (file-menu:new-help-string)))))
   (sequence (file-menu:between-new-and-open (get-file-menu)))
   (private
    (file-menu:open-item
     (and file-menu:open
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Open"
             (file-menu:open-string)
             "...")
            (get-file-menu)
            file-menu:open
            (if (preferences:get 'framework:menu-bindings) #\o #f)
            (file-menu:open-help-string)))))
   (sequence (file-menu:between-open-and-revert (get-file-menu)))
   (private
    (file-menu:revert-item
     (and file-menu:revert
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Revert"
             (file-menu:revert-string)
             "")
            (get-file-menu)
            file-menu:revert
            (if (preferences:get 'framework:menu-bindings) #f #f)
            (file-menu:revert-help-string)))))
   (sequence (file-menu:between-revert-and-save (get-file-menu)))
   (private
    (file-menu:save-item
     (and file-menu:save
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Save"
             (file-menu:save-string)
             "")
            (get-file-menu)
            file-menu:save
            (if (preferences:get 'framework:menu-bindings) #\s #f)
            (file-menu:save-help-string)))))
   (private
    (file-menu:save-as-item
     (and file-menu:save-as
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "Save"
             (file-menu:save-as-string)
             " &As...")
            (get-file-menu)
            file-menu:save-as
            (if (preferences:get 'framework:menu-bindings) #f #f)
            (file-menu:save-as-help-string)))))
   (sequence (file-menu:between-save-as-and-print (get-file-menu)))
   (private
    (file-menu:print-item
     (and file-menu:print
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Print"
             (file-menu:print-string)
             "...")
            (get-file-menu)
            file-menu:print
            (if (preferences:get 'framework:menu-bindings) #\p #f)
            (file-menu:print-help-string)))))
   (sequence (file-menu:between-print-and-close (get-file-menu)))
   (private
    (file-menu:close-item
     (and file-menu:close
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Close"
             (file-menu:close-string)
             "")
            (get-file-menu)
            file-menu:close
            (if (preferences:get 'framework:menu-bindings) #\w #f)
            (file-menu:close-help-string)))))
   (sequence (file-menu:between-close-and-quit (get-file-menu)))
   (private
    (file-menu:quit-item
     (and file-menu:quit
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             (if (eq? (system-type) 'windows) "E&xit" "Quit")
             (file-menu:quit-string)
             "")
            (get-file-menu)
            file-menu:quit
            (if (preferences:get 'framework:menu-bindings) #\q #f)
            (file-menu:quit-help-string)))))
   (sequence (file-menu:after-quit (get-file-menu)))
   (private
    (edit-menu:undo-item
     (and edit-menu:undo
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Undo"
             (edit-menu:undo-string)
             "")
            (get-edit-menu)
            edit-menu:undo
            (if (preferences:get 'framework:menu-bindings) #\z #f)
            (edit-menu:undo-help-string)))))
   (private
    (edit-menu:redo-item
     (and edit-menu:redo
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Redo"
             (edit-menu:redo-string)
             "")
            (get-edit-menu)
            edit-menu:redo
            (if (preferences:get 'framework:menu-bindings) #\y #f)
            (edit-menu:redo-help-string)))))
   (sequence (edit-menu:between-redo-and-cut (get-edit-menu)))
   (private
    (edit-menu:cut-item
     (and edit-menu:cut
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "Cu&t"
             (edit-menu:cut-string)
             "")
            (get-edit-menu)
            edit-menu:cut
            (if (preferences:get 'framework:menu-bindings) #\x #f)
            (edit-menu:cut-help-string)))))
   (sequence (edit-menu:between-cut-and-copy (get-edit-menu)))
   (private
    (edit-menu:copy-item
     (and edit-menu:copy
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Copy"
             (edit-menu:copy-string)
             "")
            (get-edit-menu)
            edit-menu:copy
            (if (preferences:get 'framework:menu-bindings) #\c #f)
            (edit-menu:copy-help-string)))))
   (sequence (edit-menu:between-copy-and-paste (get-edit-menu)))
   (private
    (edit-menu:paste-item
     (and edit-menu:paste
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "&Paste"
             (edit-menu:paste-string)
             "")
            (get-edit-menu)
            edit-menu:paste
            (if (preferences:get 'framework:menu-bindings) #\v #f)
            (edit-menu:paste-help-string)))))
   (sequence (edit-menu:between-paste-and-clear (get-edit-menu)))
   (private
    (edit-menu:clear-item
     (and edit-menu:clear
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             (if (eq? (system-type) 'macos) "Clear" "&Delete")
             (edit-menu:clear-string)
             "")
            (get-edit-menu)
            edit-menu:clear
            (if (preferences:get 'framework:menu-bindings) #f #f)
            (edit-menu:clear-help-string)))))
   (sequence (edit-menu:between-clear-and-select-all (get-edit-menu)))
   (private
    (edit-menu:select-all-item
     (and edit-menu:select-all
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "Select A&ll"
             (edit-menu:select-all-string)
             "")
            (get-edit-menu)
            edit-menu:select-all
            (if (preferences:get 'framework:menu-bindings) #\a #f)
            (edit-menu:select-all-help-string)))))
   (sequence (edit-menu:between-select-all-and-find (get-edit-menu)))
   (private
    (edit-menu:find-item
     (and edit-menu:find
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "Find"
             (edit-menu:find-string)
             "")
            (get-edit-menu)
            edit-menu:find
            (if (preferences:get 'framework:menu-bindings) #\f #f)
            (edit-menu:find-help-string)))))
   (sequence (edit-menu:between-find-and-preferences (get-edit-menu)))
   (private
    (edit-menu:preferences-item
     (and edit-menu:preferences
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "Preferences..."
             (edit-menu:preferences-string)
             "")
            (get-edit-menu)
            edit-menu:preferences
            (if (preferences:get 'framework:menu-bindings) #f #f)
            (edit-menu:preferences-help-string)))))
   (sequence (edit-menu:after-preferences (get-edit-menu)))
   (sequence (help-menu:before-about (get-help-menu)))
   (private
    (help-menu:about-item
     (and help-menu:about
          (make-object (get-menu-item%)
            ((lambda (base special suffix)
               (if (string=? special "")
                 (string-append base suffix)
                 (string-append base " " special suffix)))
             "About "
             (help-menu:about-string)
             "...")
            (get-help-menu)
            help-menu:about
            (if (preferences:get 'framework:menu-bindings) #f #f)
            (help-menu:about-help-string)))))
   (sequence (help-menu:after-about (get-help-menu)))
   (sequence (reorder-menus this))))
