(module sigs mzscheme
  (require (lib "unitsig.ss"))
  (provide gui^ script^ file-system^ code-engine^)
  (define-signature file-system^ 
    (file-inspector
     make-file-from-path
     file-name
     file-dir
     file-full-path
     file?
     file=?
     is-directory?
     directory-list
     new-file
     rename-file
     move-file
     delete-file
     copy-file))
  (define-signature gui^ 
    (selection-updated
     selection-added
     selection-removed
     get-current-directory
     change-dir
     add-window
     close-window
     toolbar-add
     toolbar-spacer))
  (define-signature script^ 
    (make-file
     file-name
     file-dir
     file-full-path
     file?
     file=?
     
     get-current-dir
     
     print-selection
     clear-selection
     cons-selection
     remove-selection 
     filter-selection
     map-selection

     directory-list
     is-directory?
     delete-file 
     new-file
     new-directory
     rename-file
     move-file
     copy-file
     copy
     cut
     paste
     
     edit-scheme
     change-dir
     open-dir-window
     close-dir-window
     toolbar-add
     toolbar-spacer))
  (define-signature code-engine^ (user-eval get-user-value open-drscheme)))