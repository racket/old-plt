(unit/sig drscheme:main^
  (import [top-level : (program argv get-dropped-files)]
          mred^
	  [fw : framework^]
	  [pretty-print : mzlib:pretty-print^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [basis : plt:basis^]
	  mzlib:function^
          mzlib:file^)


  (fw:finder:default-extension "scm")

  ;; add the graphical settings
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme without Debugging (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme without Debugging (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred)
     s)
   3)
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred-debug)
     s)
   3)

  (fw:application:current-app-name "DrScheme")
  (fw:version:add-spec 'd 1)
  
  
  ;; no more extension after this point
  (drscheme:get/extend:get-interactions-canvas%)
  (drscheme:get/extend:get-definitions-canvas%)
  (drscheme:get/extend:get-unit-frame%)
  (drscheme:get/extend:get-interactions-text%)
  (drscheme:get/extend:get-definitions-text%)

  ;; the initial window doesn't set the 
  ;; unit object's state correctly, yet.
  (define (make-basic)
    (let* ([frame (drscheme:unit:open-drscheme-window)])

      (let* ([interactions-edit (ivar frame interactions-text)]
	     [definitions-edit (ivar frame interactions-text)]
	     [filename (send definitions-edit get-filename)])
	(unless filename
	  (send interactions-edit reset-console)
	  (send interactions-edit insert-prompt)
	  (send frame update-shown)
	  (send (ivar frame interactions-canvas) focus)))
      (send frame show #t)))

  (define (remove-duplicates files)
    (let loop ([files files])
      (cond
        [(null? files) null]
        [else (if (member (car files) (cdr files))
                  (loop (cdr files))
                  (cons (car files) (loop (cdr files))))])))

  (let* ([files-to-open (append (reverse (top-level:get-dropped-files))
                                (reverse (vector->list top-level:argv)))]
         [normalized/filtered
          (let loop ([files files-to-open])
            (cond
              [(null? files) null]
              [else (let ([file (car files)])
                      (if (file-exists? file)
                          (cons (normalize-path file) (loop (cdr files)))
                          (begin
                            (message-box
                             "DrScheme"
                             (format "Cannot open ~a becuase it does not exist" file))
                            (loop (cdr files)))))]))]
         [no-dups (remove-duplicates normalized/filtered)])
    (if (null? no-dups)
	(make-basic)
	(for-each drscheme:unit:open-drscheme-window no-dups)))


  ;;
  ;; Show about box when version changes
  ;; 

  (fw:preferences:set-default 'drscheme:last-version #f
			      (lambda (x)
				(or (string? x)
				    (not x))))
  (drscheme:app:check-new-version)

  (fw:handler:insert-format-handler 
   "Projects"
   (lambda (filename) 
     (and (equal? ".plt" (filename-extension filename))
	  (gui-utils:get-choice (format "Install ~a?" filename)
				"Yes" "No")))
   (lambda (x)
     (message-box "hi" "hi"))))
