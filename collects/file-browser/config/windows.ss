(module windows (lib "mzrestricted.ss" "file-browser")
  (require (lib "script.ss" "file-browser")
           (lib "process.ss"))
  (provide (all-defined))

  
  (define (single-mouse file button keys)
    (if (not (cadr keys))
        (clear-selection))
    (case button
      ((left)
       (cons-selection file))))

  (define single-mouse-file single-mouse)
  (define single-mouse-dir single-mouse)
  
  (define (double-mouse-file file keys)
    (if (regexp-match ".*\\.ss$" (file-name file))
        (edit-scheme file)
        (process (format "emacs ~a" (file-full-path file)))))
  
  (define (box-select-file file)
    (cons-selection file))

  (define (double-mouse-dir file keys)
    (change-dir file))
  
  (define (box-select-dir file)
    (cons-selection file))
  
  (define filter-files all-files)
  
  (define sort-files dirs-first)
  
  (toolbar-spacer)
  (add-new-window-button)
  (add-close-button)
  (toolbar-spacer)
  (add-copy-button)
  (add-cut-button)
  (add-paste-button)
  )