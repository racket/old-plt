(define standard-menus<%>
  (interface
    (basic<%>)
    get-menu%
    get-menu-item%
    get-file-menu
    get-edit-menu
    get-windows-menu
    get-help-menu
    file-menu:new
    file-menu:new-string
    file-menu:new-help-string
    file-menu:between-new-and-open
    file-menu:open
    file-menu:open-string
    file-menu:open-help-string
    file-menu:between-open-and-revert
    file-menu:revert
    file-menu:revert-string
    file-menu:revert-help-string
    file-menu:between-revert-and-save
    file-menu:save
    file-menu:save-string
    file-menu:save-help-string
    file-menu:save-as
    file-menu:save-as-string
    file-menu:save-as-help-string
    file-menu:between-save-as-and-print
    file-menu:print
    file-menu:print-string
    file-menu:print-help-string
    file-menu:between-print-and-close
    file-menu:close
    file-menu:close-string
    file-menu:close-help-string
    file-menu:between-close-and-quit
    file-menu:quit
    file-menu:quit-string
    file-menu:quit-help-string
    file-menu:after-quit
    edit-menu:undo
    edit-menu:undo-string
    edit-menu:undo-help-string
    edit-menu:redo
    edit-menu:redo-string
    edit-menu:redo-help-string
    edit-menu:between-redo-and-cut
    edit-menu:cut
    edit-menu:cut-string
    edit-menu:cut-help-string
    edit-menu:between-cut-and-copy
    edit-menu:copy
    edit-menu:copy-string
    edit-menu:copy-help-string
    edit-menu:between-copy-and-paste
    edit-menu:paste
    edit-menu:paste-string
    edit-menu:paste-help-string
    edit-menu:between-paste-and-clear
    edit-menu:clear
    edit-menu:clear-string
    edit-menu:clear-help-string
    edit-menu:between-clear-and-select-all
    edit-menu:select-all
    edit-menu:select-all-string
    edit-menu:select-all-help-string
    edit-menu:between-select-all-and-find
    edit-menu:find
    edit-menu:find-string
    edit-menu:find-help-string
    edit-menu:between-find-and-preferences
    edit-menu:preferences
    edit-menu:preferences-string
    edit-menu:preferences-help-string
    edit-menu:after-preferences
    help-menu:about
    help-menu:about-string
    help-menu:about-help-string
    help-menu:after-about))

(define standard-menus-mixin
  (mixin (basic<%>)
         (standard-menus<%>)
         args
         (inherit get-menu-bar on-close show)
         (sequence (apply super-init args))
         (public (get-menu% (lambda () menu%)))
         (public (get-menu-item% (lambda () menu-item%)))
         (public (get-file-menu
                  (let ((m (make-object (get-menu%)
                             (if (eq? (system-type) 'windows) "&File" "F&ile")
                             (get-menu-bar))))
                    (lambda () m))))
         (public (get-edit-menu
                  (let ((m (make-object (get-menu%) "&Edit" (get-menu-bar))))
                    (lambda () m))))
         (public (get-windows-menu
                  (let ((m (make-object (get-menu%)
                             "&Windows"
                             (get-menu-bar))))
                    (lambda () m))))
         (public (get-help-menu
                  (let ((m (make-object (get-menu%) "&Help" (get-menu-bar))))
                    (lambda () m))))
         (public (file-menu:new
                  (lambda (item control) (handler:edit-file #f) #t))
                 (file-menu:new-string (lambda () ""))
                 (file-menu:new-help-string (lambda () "Open a new file")))
         (public (file-menu:between-new-and-open (lambda (menu) (void))))
         (public (file-menu:open
                  (lambda (item control) (handler:open-file) #t))
                 (file-menu:open-string (lambda () ""))
                 (file-menu:open-help-string
                  (lambda () "Open a file from disk")))
         (public (file-menu:between-open-and-revert (lambda (menu) (void))))
         (public (file-menu:revert #f)
                 (file-menu:revert-string (lambda () ""))
                 (file-menu:revert-help-string
                  (lambda () "Revert this file to the copy on disk")))
         (public (file-menu:between-revert-and-save (lambda (menu) (void))))
         (public (file-menu:save #f)
                 (file-menu:save-string (lambda () ""))
                 (file-menu:save-help-string
                  (lambda () "Save this file to disk")))
         (public (file-menu:save-as #f)
                 (file-menu:save-as-string (lambda () ""))
                 (file-menu:save-as-help-string
                  (lambda ()
                    "Prompt for a filename and save this file to disk")))
         (public (file-menu:between-save-as-and-print
                  (lambda (menu) (make-object separator-menu-item% menu))))
         (public (file-menu:print #f)
                 (file-menu:print-string (lambda () ""))
                 (file-menu:print-help-string (lambda () "Print this file")))
         (public (file-menu:between-print-and-close
                  (lambda (menu) (make-object separator-menu-item% menu))))
         (public (file-menu:close
                  (lambda (item control) (when (on-close) (show #f)) #t))
                 (file-menu:close-string (lambda () ""))
                 (file-menu:close-help-string (lambda () "Close this file")))
         (public (file-menu:between-close-and-quit (lambda (menu) (void))))
         (public (file-menu:quit (lambda (item control) (exit:exit)))
                 (file-menu:quit-string (lambda () ""))
                 (file-menu:quit-help-string (lambda () "Quit")))
         (public (file-menu:after-quit (lambda (menu) (void))))
         (public (edit-menu:undo #f)
                 (edit-menu:undo-string (lambda () ""))
                 (edit-menu:undo-help-string
                  (lambda () "Undo the most recent action")))
         (public (edit-menu:redo #f)
                 (edit-menu:redo-string (lambda () ""))
                 (edit-menu:redo-help-string
                  (lambda () "Redo the most recent undo")))
         (public (edit-menu:between-redo-and-cut (lambda (menu) (void))))
         (public (edit-menu:cut #f)
                 (edit-menu:cut-string (lambda () ""))
                 (edit-menu:cut-help-string (lambda () "Cut the selection")))
         (public (edit-menu:between-cut-and-copy (lambda (menu) (void))))
         (public (edit-menu:copy #f)
                 (edit-menu:copy-string (lambda () ""))
                 (edit-menu:copy-help-string (lambda () "Copy the selection")))
         (public (edit-menu:between-copy-and-paste (lambda (menu) (void))))
         (public (edit-menu:paste #f)
                 (edit-menu:paste-string (lambda () ""))
                 (edit-menu:paste-help-string
                  (lambda ()
                    "Paste the most recent copy or cut over the selection")))
         (public (edit-menu:between-paste-and-clear (lambda (menu) (void))))
         (public (edit-menu:clear #f)
                 (edit-menu:clear-string (lambda () ""))
                 (edit-menu:clear-help-string
                  (lambda () "Clear the selection without affecting paste")))
         (public (edit-menu:between-clear-and-select-all
                  (lambda (menu) (void))))
         (public (edit-menu:select-all #f)
                 (edit-menu:select-all-string (lambda () ""))
                 (edit-menu:select-all-help-string
                  (lambda () "Select the entire document")))
         (public (edit-menu:between-select-all-and-find
                  (lambda (menu) (void))))
         (public (edit-menu:find
                  (lambda (item control)
                    (send this move-to-search-or-search)
                    #t))
                 (edit-menu:find-string (lambda () ""))
                 (edit-menu:find-help-string
                  (lambda () "Search for a string in the window")))
         (public (edit-menu:between-find-and-preferences
                  (lambda (menu) (make-object separator-menu-item% menu))))
         (public (edit-menu:preferences
                  (lambda (item control) (preferences:show-dialog) #t))
                 (edit-menu:preferences-string (lambda () ""))
                 (edit-menu:preferences-help-string
                  (lambda () "Configure the preferences")))
         (public (edit-menu:after-preferences (lambda (menu) (void))))
         (public (help-menu:about #f)
                 (help-menu:about-string (lambda () ""))
                 (help-menu:about-help-string
                  (lambda () "Learn something about this application")))
         (public (help-menu:after-about (lambda (menu) (void))))
         (sequence (void))
         (sequence (void))
         (sequence (void))
         (sequence (void))
         (sequence (void))
         (sequence (void))
         (public (file-menu:new-menu
                  (and file-menu:new
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&New"
                          file-menu:new-string
                          "")
                         (get-file-menu)
                         file-menu:new
                         #\n
                         (file-menu:new-help-string)))))
         (sequence (file-menu:between-new-and-open (get-file-menu)))
         (public (file-menu:open-menu
                  (and file-menu:open
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Open"
                          file-menu:open-string
                          "...")
                         (get-file-menu)
                         file-menu:open
                         #\o
                         (file-menu:open-help-string)))))
         (sequence (file-menu:between-open-and-revert (get-file-menu)))
         (public (file-menu:revert-menu
                  (and file-menu:revert
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Revert"
                          file-menu:revert-string
                          "")
                         (get-file-menu)
                         file-menu:revert
                         #f
                         (file-menu:revert-help-string)))))
         (sequence (file-menu:between-revert-and-save (get-file-menu)))
         (public (file-menu:save-menu
                  (and file-menu:save
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Save"
                          file-menu:save-string
                          "")
                         (get-file-menu)
                         file-menu:save
                         "s"
                         (file-menu:save-help-string)))))
         (public (file-menu:save-as-menu
                  (and file-menu:save-as
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "Save"
                          file-menu:save-as-string
                          " &As...")
                         (get-file-menu)
                         file-menu:save-as
                         #f
                         (file-menu:save-as-help-string)))))
         (sequence (file-menu:between-save-as-and-print (get-file-menu)))
         (public (file-menu:print-menu
                  (and file-menu:print
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Print"
                          file-menu:print-string
                          "...")
                         (get-file-menu)
                         file-menu:print
                         "p"
                         (file-menu:print-help-string)))))
         (sequence (file-menu:between-print-and-close (get-file-menu)))
         (public (file-menu:close-menu
                  (and file-menu:close
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Close"
                          file-menu:close-string
                          "")
                         (get-file-menu)
                         file-menu:close
                         #\w
                         (file-menu:close-help-string)))))
         (sequence (file-menu:between-close-and-quit (get-file-menu)))
         (public (file-menu:quit-menu
                  (and file-menu:quit
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          (if (eq? (system-type) 'windows) "E&xit" "Quit")
                          file-menu:quit-string
                          "")
                         (get-file-menu)
                         file-menu:quit
                         #\q
                         (file-menu:quit-help-string)))))
         (sequence (file-menu:after-quit (get-file-menu)))
         (public (edit-menu:undo-menu
                  (and edit-menu:undo
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Undo"
                          edit-menu:undo-string
                          "")
                         (get-edit-menu)
                         edit-menu:undo
                         #\z
                         (edit-menu:undo-help-string)))))
         (public (edit-menu:redo-menu
                  (and edit-menu:redo
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Redo"
                          edit-menu:redo-string
                          "")
                         (get-edit-menu)
                         edit-menu:redo
                         #\y
                         (edit-menu:redo-help-string)))))
         (sequence (edit-menu:between-redo-and-cut (get-edit-menu)))
         (public (edit-menu:cut-menu
                  (and edit-menu:cut
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "Cu&t"
                          edit-menu:cut-string
                          "")
                         (get-edit-menu)
                         edit-menu:cut
                         #\x
                         (edit-menu:cut-help-string)))))
         (sequence (edit-menu:between-cut-and-copy (get-edit-menu)))
         (public (edit-menu:copy-menu
                  (and edit-menu:copy
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Copy"
                          edit-menu:copy-string
                          "")
                         (get-edit-menu)
                         edit-menu:copy
                         #\c
                         (edit-menu:copy-help-string)))))
         (sequence (edit-menu:between-copy-and-paste (get-edit-menu)))
         (public (edit-menu:paste-menu
                  (and edit-menu:paste
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "&Paste"
                          edit-menu:paste-string
                          "")
                         (get-edit-menu)
                         edit-menu:paste
                         #\v
                         (edit-menu:paste-help-string)))))
         (sequence (edit-menu:between-paste-and-clear (get-edit-menu)))
         (public (edit-menu:clear-menu
                  (and edit-menu:clear
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          (if (eq? (system-type) 'macos) "Clear" "&Delete")
                          edit-menu:clear-string
                          "")
                         (get-edit-menu)
                         edit-menu:clear
                         #f
                         (edit-menu:clear-help-string)))))
         (sequence (edit-menu:between-clear-and-select-all (get-edit-menu)))
         (public (edit-menu:select-all-menu
                  (and edit-menu:select-all
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "Select A&ll"
                          edit-menu:select-all-string
                          "")
                         (get-edit-menu)
                         edit-menu:select-all
                         #\a
                         (edit-menu:select-all-help-string)))))
         (sequence (edit-menu:between-select-all-and-find (get-edit-menu)))
         (public (edit-menu:find-menu
                  (and edit-menu:find
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "Find"
                          edit-menu:find-string
                          "")
                         (get-edit-menu)
                         edit-menu:find
                         #\f
                         (edit-menu:find-help-string)))))
         (sequence (edit-menu:between-find-and-preferences (get-edit-menu)))
         (public (edit-menu:preferences-menu
                  (and edit-menu:preferences
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "Preferences..."
                          edit-menu:preferences-string
                          "")
                         (get-edit-menu)
                         edit-menu:preferences
                         #f
                         (edit-menu:preferences-help-string)))))
         (sequence (edit-menu:after-preferences (get-edit-menu)))
         (public (help-menu:about-menu
                  (and help-menu:about
                       (make-object (get-menu-item%)
                         ((lambda (base special suffix)
                            (if (string=? special "")
                              (string-append base suffix)
                              (string-append base " " special suffix)))
                          "About "
                          help-menu:about-string
                          "...")
                         (get-help-menu)
                         help-menu:about
                         #f
                         (help-menu:about-help-string)))))
         (sequence (help-menu:after-about (get-help-menu)))))
