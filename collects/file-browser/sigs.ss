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
     delete-file))
  (define-signature gui^ 
    (selection-updated
     selection-added
     selection-removed
     get-current-directory
     add-window
     close-window))
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

     open-dir-window
     close-dir-window))
  (define-signature code-engine^ (user-thread-eval get-user-value)))