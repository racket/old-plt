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
    file-menu:open-recent-callback
    file-menu:get-open-recent-item
    file-menu:open-recent-string
    file-menu:open-recent-help-string
    file-menu:open-recent-on-demand
    file-menu:create-open-recent?
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
   (inherit on-menu-char on-traverse-char)
   (define remove-prefs-callback
     (preferences:add-callback
       'framework:menu-bindings
       (lambda (p v)
         (let loop ((menu (get-menu-bar)))
           (when (is-a? menu menu:can-restore<%>)
             (if v (send menu restore-keybinding) (send menu set-shortcut #f)))
           (when (is-a? menu menu:can-restore-underscore<%>)
             (if v
               (send menu restore-underscores)
               (send menu erase-underscores)))
           (when (is-a? menu menu-item-container<%>)
             (for-each loop (send menu get-items)))))))
   (inherit get-menu-bar show can-close? get-edit-target-object)
   (define/override
     on-close
     (lambda () (remove-prefs-callback) (super-on-close)))
   (define/public get-menu% (lambda () menu:can-restore-underscore-menu%))
   (define/public get-menu-item% (lambda () menu:can-restore-menu-item%))
   (define/public
     get-checkable-menu-item%
     (lambda () menu:can-restore-checkable-menu-item%))
   (define/public get-file-menu (lambda () file-menu))
   (define/public get-edit-menu (lambda () edit-menu))
   (define/public get-help-menu (lambda () help-menu))
   (define/public
     file-menu:new-callback
     (lambda (item control) (handler:edit-file #f) #t))
   (define/public file-menu:get-new-item (lambda () file-menu:new-item))
   (define/public
     file-menu:new-string
     (lambda () (string-constant new-menu-item)))
   (define/public
     file-menu:new-help-string
     (lambda () (string-constant new-info)))
   (define/public file-menu:new-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-new? (lambda () #t))
   (define/public file-menu:between-new-and-open (lambda (menu) (void)))
   (define/public
     file-menu:open-callback
     (lambda (item control) (handler:open-file) #t))
   (define/public file-menu:get-open-item (lambda () file-menu:open-item))
   (define/public
     file-menu:open-string
     (lambda () (string-constant open-menu-item)))
   (define/public
     file-menu:open-help-string
     (lambda () (string-constant open-info)))
   (define/public file-menu:open-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-open? (lambda () #t))
   (define/public file-menu:open-recent-callback (lambda (x y) (void)))
   (define/public
     file-menu:get-open-recent-item
     (lambda () file-menu:open-recent-item))
   (define/public
     file-menu:open-recent-string
     (lambda () (string-constant open-recent-menu-item)))
   (define/public
     file-menu:open-recent-help-string
     (lambda () (string-constant open-recent-info)))
   (define/public
     file-menu:open-recent-on-demand
     (lambda (menu) (handler:install-recent-items menu)))
   (define/public file-menu:create-open-recent? (lambda () #t))
   (define/public file-menu:between-open-and-revert (lambda (menu) (void)))
   (define/public file-menu:revert-callback (lambda (item control) (void)))
   (define/public file-menu:get-revert-item (lambda () file-menu:revert-item))
   (define/public
     file-menu:revert-string
     (lambda () (string-constant revert-menu-item)))
   (define/public
     file-menu:revert-help-string
     (lambda () (string-constant revert-info)))
   (define/public file-menu:revert-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-revert? (lambda () #f))
   (define/public file-menu:between-revert-and-save (lambda (menu) (void)))
   (define/public file-menu:save-callback (lambda (item control) (void)))
   (define/public file-menu:get-save-item (lambda () file-menu:save-item))
   (define/public
     file-menu:save-string
     (lambda () (string-constant save-menu-item)))
   (define/public
     file-menu:save-help-string
     (lambda () (string-constant save-info)))
   (define/public file-menu:save-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-save? (lambda () #f))
   (define/public file-menu:save-as-callback (lambda (item control) (void)))
   (define/public
     file-menu:get-save-as-item
     (lambda () file-menu:save-as-item))
   (define/public
     file-menu:save-as-string
     (lambda () (string-constant save-as-menu-item)))
   (define/public
     file-menu:save-as-help-string
     (lambda () (string-constant save-as-info)))
   (define/public file-menu:save-as-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-save-as? (lambda () #f))
   (define/public file-menu:between-save-as-and-print (lambda (menu) (void)))
   (define/public file-menu:print-callback (lambda (item control) (void)))
   (define/public file-menu:get-print-item (lambda () file-menu:print-item))
   (define/public
     file-menu:print-string
     (lambda () (string-constant print-menu-item)))
   (define/public
     file-menu:print-help-string
     (lambda () (string-constant print-info)))
   (define/public file-menu:print-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-print? (lambda () #f))
   (define/public
     file-menu:between-print-and-close
     (lambda (menu) (make-object separator-menu-item% menu)))
   (define/public
     file-menu:close-callback
     (lambda (item control) (when (can-close?) (on-close) (show #f)) #t))
   (define/public file-menu:get-close-item (lambda () file-menu:close-item))
   (define/public
     file-menu:close-string
     (lambda () (string-constant close-menu-item)))
   (define/public
     file-menu:close-help-string
     (lambda () (string-constant close-info)))
   (define/public file-menu:close-on-demand (lambda (menu-item) (void)))
   (define/public file-menu:create-close? (lambda () #t))
   (define/public file-menu:between-close-and-quit (lambda (menu) (void)))
   (define/public file-menu:quit-callback (lambda (item control) (exit:exit)))
   (define/public file-menu:get-quit-item (lambda () file-menu:quit-item))
   (define/public
     file-menu:quit-string
     (lambda ()
       (if (eq? (system-type) 'windows)
         (string-constant quit-menu-item-windows)
         (string-constant quit-menu-item-others))))
   (define/public
     file-menu:quit-help-string
     (lambda () (string-constant quit-info)))
   (define/public file-menu:quit-on-demand (lambda (menu-item) (void)))
   (define/public
     file-menu:create-quit?
     (lambda () (not (current-eventspace-has-standard-menus?))))
   (define/public file-menu:after-quit (lambda (menu) (void)))
   (define/public
     edit-menu:undo-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'undo)))
       #t))
   (define/public edit-menu:get-undo-item (lambda () edit-menu:undo-item))
   (define/public
     edit-menu:undo-string
     (lambda () (string-constant undo-menu-item)))
   (define/public
     edit-menu:undo-help-string
     (lambda () (string-constant undo-info)))
   (define/public
     edit-menu:undo-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'undo))))
         (send item enable enable?))))
   (define/public edit-menu:create-undo? (lambda () #t))
   (define/public
     edit-menu:redo-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'redo)))
       #t))
   (define/public edit-menu:get-redo-item (lambda () edit-menu:redo-item))
   (define/public
     edit-menu:redo-string
     (lambda () (string-constant redo-menu-item)))
   (define/public
     edit-menu:redo-help-string
     (lambda () (string-constant redo-info)))
   (define/public
     edit-menu:redo-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'redo))))
         (send item enable enable?))))
   (define/public edit-menu:create-redo? (lambda () #t))
   (define/public
     edit-menu:between-redo-and-cut
     (lambda (menu) (make-object separator-menu-item% menu)))
   (define/public
     edit-menu:cut-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'cut)))
       #t))
   (define/public edit-menu:get-cut-item (lambda () edit-menu:cut-item))
   (define/public
     edit-menu:cut-string
     (lambda () (string-constant cut-menu-item)))
   (define/public
     edit-menu:cut-help-string
     (lambda () (string-constant cut-info)))
   (define/public
     edit-menu:cut-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'cut))))
         (send item enable enable?))))
   (define/public edit-menu:create-cut? (lambda () #t))
   (define/public edit-menu:between-cut-and-copy (lambda (menu) (void)))
   (define/public
     edit-menu:copy-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'copy)))
       #t))
   (define/public edit-menu:get-copy-item (lambda () edit-menu:copy-item))
   (define/public
     edit-menu:copy-string
     (lambda () (string-constant copy-menu-item)))
   (define/public
     edit-menu:copy-help-string
     (lambda () (string-constant copy-info)))
   (define/public
     edit-menu:copy-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'copy))))
         (send item enable enable?))))
   (define/public edit-menu:create-copy? (lambda () #t))
   (define/public edit-menu:between-copy-and-paste (lambda (menu) (void)))
   (define/public
     edit-menu:paste-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'paste)))
       #t))
   (define/public edit-menu:get-paste-item (lambda () edit-menu:paste-item))
   (define/public
     edit-menu:paste-string
     (lambda () (string-constant paste-menu-item)))
   (define/public
     edit-menu:paste-help-string
     (lambda () (string-constant paste-info)))
   (define/public
     edit-menu:paste-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'paste))))
         (send item enable enable?))))
   (define/public edit-menu:create-paste? (lambda () #t))
   (define/public edit-menu:between-paste-and-clear (lambda (menu) (void)))
   (define/public
     edit-menu:clear-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'clear)))
       #t))
   (define/public edit-menu:get-clear-item (lambda () edit-menu:clear-item))
   (define/public
     edit-menu:clear-string
     (lambda ()
       (if (eq? (system-type) 'windows)
         (string-constant clear-menu-item-windows)
         (string-constant clear-menu-item-windows))))
   (define/public
     edit-menu:clear-help-string
     (lambda () (string-constant clear-info)))
   (define/public
     edit-menu:clear-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'clear))))
         (send item enable enable?))))
   (define/public edit-menu:create-clear? (lambda () #t))
   (define/public
     edit-menu:between-clear-and-select-all
     (lambda (menu) (void)))
   (define/public
     edit-menu:select-all-callback
     (lambda (menu evt)
       (let ((edit (get-edit-target-object)))
         (when (and edit (is-a? edit editor<%>))
           (send edit do-edit-operation 'select-all)))
       #t))
   (define/public
     edit-menu:get-select-all-item
     (lambda () edit-menu:select-all-item))
   (define/public
     edit-menu:select-all-string
     (lambda () (string-constant select-all-menu-item)))
   (define/public
     edit-menu:select-all-help-string
     (lambda () (string-constant select-all-info)))
   (define/public
     edit-menu:select-all-on-demand
     (lambda (item)
       (let* ((editor (get-edit-target-object))
              (enable?
                (and editor
                     (is-a? editor editor<%>)
                     (send editor can-do-edit-operation? 'select-all))))
         (send item enable enable?))))
   (define/public edit-menu:create-select-all? (lambda () #t))
   (define/public
     edit-menu:between-select-all-and-find
     (lambda (menu) (make-object separator-menu-item% menu)))
   (define/public edit-menu:find-callback (lambda (item control) (void)))
   (define/public edit-menu:get-find-item (lambda () edit-menu:find-item))
   (define/public
     edit-menu:find-string
     (lambda () (string-constant find-menu-item)))
   (define/public
     edit-menu:find-help-string
     (lambda () (string-constant find-info)))
   (define/public
     edit-menu:find-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
   (define/public edit-menu:create-find? (lambda () #f))
   (define/public edit-menu:find-again-callback (lambda (item control) (void)))
   (define/public
     edit-menu:get-find-again-item
     (lambda () edit-menu:find-again-item))
   (define/public
     edit-menu:find-again-string
     (lambda () (string-constant find-again-menu-item)))
   (define/public
     edit-menu:find-again-help-string
     (lambda () (string-constant find-again-info)))
   (define/public
     edit-menu:find-again-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
   (define/public edit-menu:create-find-again? (lambda () #f))
   (define/public
     edit-menu:replace-and-find-again-callback
     (lambda (item control) (void)))
   (define/public
     edit-menu:get-replace-and-find-again-item
     (lambda () edit-menu:replace-and-find-again-item))
   (define/public
     edit-menu:replace-and-find-again-string
     (lambda () (string-constant replace-and-find-again-menu-item)))
   (define/public
     edit-menu:replace-and-find-again-help-string
     (lambda () (string-constant replace-and-find-again-info)))
   (define/public
     edit-menu:replace-and-find-again-on-demand
     (lambda (item)
       (send item enable
         (let
          ((target (get-edit-target-object)))
          (and target (is-a? target editor<%>))))))
   (define/public edit-menu:create-replace-and-find-again? (lambda () #f))
   (define/public
     edit-menu:between-find-and-preferences
     (lambda (menu)
       (unless (current-eventspace-has-standard-menus?)
         (make-object separator-menu-item% menu))))
   (define/public
     edit-menu:preferences-callback
     (lambda (item control) (preferences:show-dialog) #t))
   (define/public
     edit-menu:get-preferences-item
     (lambda () edit-menu:preferences-item))
   (define/public
     edit-menu:preferences-string
     (lambda () (string-constant preferences-menu-item)))
   (define/public
     edit-menu:preferences-help-string
     (lambda () (string-constant preferences-info)))
   (define/public edit-menu:preferences-on-demand (lambda (menu-item) (void)))
   (define/public
     edit-menu:create-preferences?
     (lambda () (not (current-eventspace-has-standard-menus?))))
   (define/public edit-menu:after-preferences (lambda (menu) (void)))
   (define/public help-menu:before-about (lambda (menu) (void)))
   (define/public help-menu:about-callback (lambda (item control) (void)))
   (define/public help-menu:get-about-item (lambda () help-menu:about-item))
   (define/public
     help-menu:about-string
     (lambda () (string-constant about-menu-item)))
   (define/public
     help-menu:about-help-string
     (lambda () (string-constant about-info)))
   (define/public help-menu:about-on-demand (lambda (menu-item) (void)))
   (define/public help-menu:create-about? (lambda () #f))
   (define/public help-menu:after-about (lambda (menu) (void)))
   (super-instantiate ())
   (rename (super-on-close on-close))
   (define file-menu
     (make-object (get-menu%)
       (if
        (eq? (system-type) 'windows)
        (string-constant file-menu-label-windows)
        (string-constant file-menu-label-other))
       (get-menu-bar)))
   (define edit-menu
     (make-object (get-menu%)
       (string-constant edit-menu-label)
       (get-menu-bar)))
   (define help-menu
     (make-object (get-menu%)
       (string-constant help-menu-label)
       (get-menu-bar)))
   (define file-menu:new-item
     (and (file-menu:create-new?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:new-string))
            (parent file-menu)
            (callback
             (let ((file-menu:new-callback
                     (lambda (item evt) (file-menu:new-callback item evt))))
               file-menu:new-callback))
            (shortcut #\n)
            (help-string (file-menu:new-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:new-on-demand menu-item))))))
   (file-menu:between-new-and-open (get-file-menu))
   (define file-menu:open-item
     (and (file-menu:create-open?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:open-string))
            (parent file-menu)
            (callback
             (let ((file-menu:open-callback
                     (lambda (item evt) (file-menu:open-callback item evt))))
               file-menu:open-callback))
            (shortcut #\o)
            (help-string (file-menu:open-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:open-on-demand menu-item))))))
   (define file-menu:open-recent-item
     (and (file-menu:create-open-recent?)
          (instantiate (get-menu%) ()
            (label (file-menu:open-recent-string))
            (parent file-menu)
            (help-string (file-menu:open-recent-help-string))
            (demand-callback
             (lambda (menu-item)
               (file-menu:open-recent-on-demand menu-item))))))
   (file-menu:between-open-and-revert (get-file-menu))
   (define file-menu:revert-item
     (and (file-menu:create-revert?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:revert-string))
            (parent file-menu)
            (callback
             (let ((file-menu:revert-callback
                     (lambda (item evt) (file-menu:revert-callback item evt))))
               file-menu:revert-callback))
            (shortcut #f)
            (help-string (file-menu:revert-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:revert-on-demand menu-item))))))
   (file-menu:between-revert-and-save (get-file-menu))
   (define file-menu:save-item
     (and (file-menu:create-save?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:save-string))
            (parent file-menu)
            (callback
             (let ((file-menu:save-callback
                     (lambda (item evt) (file-menu:save-callback item evt))))
               file-menu:save-callback))
            (shortcut #\s)
            (help-string (file-menu:save-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:save-on-demand menu-item))))))
   (define file-menu:save-as-item
     (and (file-menu:create-save-as?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:save-as-string))
            (parent file-menu)
            (callback
             (let ((file-menu:save-as-callback
                     (lambda (item evt)
                       (file-menu:save-as-callback item evt))))
               file-menu:save-as-callback))
            (shortcut #f)
            (help-string (file-menu:save-as-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:save-as-on-demand menu-item))))))
   (file-menu:between-save-as-and-print (get-file-menu))
   (define file-menu:print-item
     (and (file-menu:create-print?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:print-string))
            (parent file-menu)
            (callback
             (let ((file-menu:print-callback
                     (lambda (item evt) (file-menu:print-callback item evt))))
               file-menu:print-callback))
            (shortcut #\p)
            (help-string (file-menu:print-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:print-on-demand menu-item))))))
   (file-menu:between-print-and-close (get-file-menu))
   (define file-menu:close-item
     (and (file-menu:create-close?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:close-string))
            (parent file-menu)
            (callback
             (let ((file-menu:close-callback
                     (lambda (item evt) (file-menu:close-callback item evt))))
               file-menu:close-callback))
            (shortcut #\w)
            (help-string (file-menu:close-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:close-on-demand menu-item))))))
   (file-menu:between-close-and-quit (get-file-menu))
   (define file-menu:quit-item
     (and (file-menu:create-quit?)
          (instantiate (get-menu-item%) ()
            (label (file-menu:quit-string))
            (parent file-menu)
            (callback
             (let ((file-menu:quit-callback
                     (lambda (item evt) (file-menu:quit-callback item evt))))
               file-menu:quit-callback))
            (shortcut #\q)
            (help-string (file-menu:quit-help-string))
            (demand-callback
             (lambda (menu-item) (file-menu:quit-on-demand menu-item))))))
   (file-menu:after-quit (get-file-menu))
   (define edit-menu:undo-item
     (and (edit-menu:create-undo?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:undo-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:undo-callback
                     (lambda (item evt) (edit-menu:undo-callback item evt))))
               edit-menu:undo-callback))
            (shortcut #\z)
            (help-string (edit-menu:undo-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:undo-on-demand menu-item))))))
   (define edit-menu:redo-item
     (and (edit-menu:create-redo?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:redo-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:redo-callback
                     (lambda (item evt) (edit-menu:redo-callback item evt))))
               edit-menu:redo-callback))
            (shortcut #\y)
            (help-string (edit-menu:redo-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:redo-on-demand menu-item))))))
   (edit-menu:between-redo-and-cut (get-edit-menu))
   (define edit-menu:cut-item
     (and (edit-menu:create-cut?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:cut-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:cut-callback
                     (lambda (item evt) (edit-menu:cut-callback item evt))))
               edit-menu:cut-callback))
            (shortcut #\x)
            (help-string (edit-menu:cut-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:cut-on-demand menu-item))))))
   (edit-menu:between-cut-and-copy (get-edit-menu))
   (define edit-menu:copy-item
     (and (edit-menu:create-copy?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:copy-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:copy-callback
                     (lambda (item evt) (edit-menu:copy-callback item evt))))
               edit-menu:copy-callback))
            (shortcut #\c)
            (help-string (edit-menu:copy-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:copy-on-demand menu-item))))))
   (edit-menu:between-copy-and-paste (get-edit-menu))
   (define edit-menu:paste-item
     (and (edit-menu:create-paste?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:paste-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:paste-callback
                     (lambda (item evt) (edit-menu:paste-callback item evt))))
               edit-menu:paste-callback))
            (shortcut #\v)
            (help-string (edit-menu:paste-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:paste-on-demand menu-item))))))
   (edit-menu:between-paste-and-clear (get-edit-menu))
   (define edit-menu:clear-item
     (and (edit-menu:create-clear?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:clear-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:clear-callback
                     (lambda (item evt) (edit-menu:clear-callback item evt))))
               edit-menu:clear-callback))
            (shortcut #f)
            (help-string (edit-menu:clear-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:clear-on-demand menu-item))))))
   (edit-menu:between-clear-and-select-all (get-edit-menu))
   (define edit-menu:select-all-item
     (and (edit-menu:create-select-all?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:select-all-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:select-all-callback
                     (lambda (item evt)
                       (edit-menu:select-all-callback item evt))))
               edit-menu:select-all-callback))
            (shortcut #\a)
            (help-string (edit-menu:select-all-help-string))
            (demand-callback
             (lambda (menu-item)
               (edit-menu:select-all-on-demand menu-item))))))
   (edit-menu:between-select-all-and-find (get-edit-menu))
   (define edit-menu:find-item
     (and (edit-menu:create-find?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:find-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:find-callback
                     (lambda (item evt) (edit-menu:find-callback item evt))))
               edit-menu:find-callback))
            (shortcut #\f)
            (help-string (edit-menu:find-help-string))
            (demand-callback
             (lambda (menu-item) (edit-menu:find-on-demand menu-item))))))
   (define edit-menu:find-again-item
     (and (edit-menu:create-find-again?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:find-again-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:find-again-callback
                     (lambda (item evt)
                       (edit-menu:find-again-callback item evt))))
               edit-menu:find-again-callback))
            (shortcut #\g)
            (help-string (edit-menu:find-again-help-string))
            (demand-callback
             (lambda (menu-item)
               (edit-menu:find-again-on-demand menu-item))))))
   (define edit-menu:replace-and-find-again-item
     (and (edit-menu:create-replace-and-find-again?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:replace-and-find-again-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:replace-and-find-again-callback
                     (lambda (item evt)
                       (edit-menu:replace-and-find-again-callback item evt))))
               edit-menu:replace-and-find-again-callback))
            (shortcut (if (eq? (system-type) 'macosx) #f #\h))
            (help-string (edit-menu:replace-and-find-again-help-string))
            (demand-callback
             (lambda (menu-item)
               (edit-menu:replace-and-find-again-on-demand menu-item))))))
   (edit-menu:between-find-and-preferences (get-edit-menu))
   (define edit-menu:preferences-item
     (and (edit-menu:create-preferences?)
          (instantiate (get-menu-item%) ()
            (label (edit-menu:preferences-string))
            (parent edit-menu)
            (callback
             (let ((edit-menu:preferences-callback
                     (lambda (item evt)
                       (edit-menu:preferences-callback item evt))))
               edit-menu:preferences-callback))
            (shortcut #\;)
            (help-string (edit-menu:preferences-help-string))
            (demand-callback
             (lambda (menu-item)
               (edit-menu:preferences-on-demand menu-item))))))
   (edit-menu:after-preferences (get-edit-menu))
   (help-menu:before-about (get-help-menu))
   (define help-menu:about-item
     (and (help-menu:create-about?)
          (instantiate (get-menu-item%) ()
            (label (help-menu:about-string))
            (parent help-menu)
            (callback
             (let ((help-menu:about-callback
                     (lambda (item evt) (help-menu:about-callback item evt))))
               help-menu:about-callback))
            (shortcut #f)
            (help-string (help-menu:about-help-string))
            (demand-callback
             (lambda (menu-item) (help-menu:about-on-demand menu-item))))))
   (help-menu:after-about (get-help-menu))
   (reorder-menus this)))
