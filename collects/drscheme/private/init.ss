
(module init mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           "drsig.ss"
           (lib "list.ss")
	   (lib "mred.ss" "mred"))
  
  (provide init@)
  
  (define init@
    (unit/sig drscheme:init^
      (import)
      
      (define original-output-port (current-output-port))
      (define original-error-port (current-error-port))
      
      (define primitive-eval (current-eval))
      (define primitive-load (current-load))
      
      (define system-custodian (current-custodian))
      (define system-eventspace (current-eventspace))
      (define system-thread (current-thread))
      (define system-namespace (current-namespace))
      (define first-dir (current-directory))
      
      (define original-error-display-handler (error-display-handler))
      
      (define error-display-handler-message-box-title
        (make-parameter (string-constant drscheme-internal-error)))
      
      ;; override error-display-handler to duplicate the error
      ;; message in both the standard place (as defined by the
      ;; current error-display-handler) and in a message box
      ;; identifying the error as a drscheme internal error.
      (error-display-handler
       (λ (msg exn)
         
         ;; this  may raise an exception if the port is gone.
         (with-handlers ([exn:fail? (λ (x) (void))])
           (original-error-display-handler msg exn))
         
         (let ([title (error-display-handler-message-box-title)])
           (let ([text (let ([p (open-output-string)])
                         (parameterize ([current-error-port p]
                                        [current-output-port p])
                           (original-error-display-handler msg exn))
                         (get-output-string p))])
             
             (if (eq? (current-eventspace) system-eventspace)
                 (message-box title text #f '(stop ok))
                 (parameterize ([current-eventspace system-eventspace]
                                [current-custodian system-custodian])
                   (queue-callback
                    (λ ()
                      (message-box title text #f '(stop ok))))))))))
      
      ;; all-toplevel-collections : -> (listof string)
      ;; returns the list of collections currently available
      (define (all-toplevel-collections)
        ;; collections-hash-table : hash-table[path -o> path]
        ;; contains a list of the available collections.
        ;; use a hash table to cancel out duplicate collections
        (define collections-hash-table (make-hash-table 'equal))
        
        ;; add-collections-in-path : path -> void
        ;; adds each collection in the given path
        ;; to collections-hash-table
        (define (add-collections-in-path path)
          (for-each 
           (λ (d) 
             (when (and (directory-exists? (build-path path d))
                        (not (string-ci=? (path->string d) "CVS")))
               (hash-table-put! collections-hash-table d d)))
           (with-handlers ([exn:fail:filesystem?
                            (λ (x) null)])
             (directory-list path))))
        
        (for-each add-collections-in-path (current-library-collection-paths))
        (quicksort
         (hash-table-map collections-hash-table (λ (x y) y))
         (λ (x y) (string<=? (path->string x) (path->string y))))))))
