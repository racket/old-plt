(unit/sig ()
  (import mred^
          mzlib:core^
          framework^
          [print-convert : mzlib:print-convert^]
          [drscheme : drscheme:export^]
          [zodiac : zodiac:system^])

  (include "multi-file-search.ss")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; hook into the drscheme frame
  ;;;

  ;; search-extenstion : (derived-from frame%) -> (derived-from frame%)
  ;; extends the drscheme frame with the menu binding for multi-file search
  (define (search-extension frame%)
    (class frame% args
      (rename [super-edit-menu:between-find-and-preferences
               edit-menu:between-find-and-preferences])
      (override
        [edit-menu:between-find-and-preferences
         (lambda (edit-menu)
           (make-object menu% "Multi-file Search" edit-menu (lambda (x y) (multi-file-search)))
           (super-edit-menu:between-find-and-preferences edit-menu))])
      (sequence (apply super-init args))))
  
  ;; void
  ;; does the actual extension
  (drscheme:get/extend:extend-unit-frame search-extension))
