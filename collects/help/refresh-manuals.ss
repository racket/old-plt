(module refresh-manuals mzscheme
  (require "private/docpos.ss"
           (lib "plt-installer.ss" "setup")
           (lib "class.ss")
           (lib "url.ss" "net")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants"))
  
  (provide refresh-manuals)
  
  (define (refresh-manuals)
    (invoke-unit/sig refresh-manuals@))

  (define known-docs-plus-help
	(cons '("help" . "PLT Help Desk")
	      known-docs))
  
  (define refresh-manuals@
    (unit/sig () 
      (import)
      
      (define doc-url-format "http://download.plt-scheme.org/doc/pre-release/bundles/~a-doc.plt")
      (define (make-local-doc-filename tmp-dir stub)
        (build-path tmp-dir (format "~a-doc.plt" stub)))
      
      (define f (make-object frame% (string-constant refreshing-manuals) #f 600))
      (define m (instantiate message% () 
                  (label "")
                  (parent f)
                  (stretchable-width #t)))
      (define bp (make-object horizontal-panel% f))
      (define cancel (make-object button% (string-constant cancel) bp
                       (lambda (x y)
                         (break-thread t))))
      (define close (make-object button% (string-constant close) bp
                      (lambda (x y)
                        (send f show #f))))
      (send close enable #f)
      (define (show-message msg) (send m set-label msg))
      (define sema (make-semaphore 0))

      (define t
        (thread
         (lambda ()
           (with-handlers ([(lambda (x) #t)
                            (lambda (x)
                              (send cancel enable #f)
                              (send close enable #t)
                              (show-message (if (exn? x)
                                                (exn-message x)
                                                (format "uncaught exn: ~s" x))))])
             (semaphore-post sema)
             (let ([tmp-directory (find/create-temporary-docs-dir)])
               (download-known-docs tmp-directory)
               (delete-known-docs)
               (install-known-docs tmp-directory)
               (delete-local-plt-files tmp-directory)
               (show-message "Finished")
               (send cancel enable #f)
               (send close enable #t))))))
    
    ;; main : -> void
    ;; thread is already running, but this shows the progress frame.
    (define (main)
      (semaphore-wait sema)
      (send f show #t))
    
          ;; find/create-temporary-docs-dir : -> string
    ;; if cannot find a suitable directory, an exn is raised.
    ;; if okay, returns the path to the directory.
    (define (find/create-temporary-docs-dir)
      (let ([temp-dir (find-system-path 'temp-dir)])
        (let loop ([n 0])
          (if (= n 15)
              (error 'find/create-temporary-docs-dir "please clean out ~a")
              (let ([candidate (build-path temp-dir (format "help-refresh-docs~a" n))])
                  (if (directory-exists? candidate)
                      (loop (+ n 1))
                      (begin
                        (make-directory candidate)
                        candidate)))))))
                  

      


                                                        
    ;;                       ;;;                     ;; 
     ;                         ;                      ; 
     ;                         ;                      ; 
  ;;;;   ;;;  ;;; ;;;; ;;;     ;     ;;;   ;;;;    ;;;; 
 ;   ;  ;   ;  ;   ;  ;;  ;    ;    ;   ;      ;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;   ;;;;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
 ;   ;  ;   ;   ; ;   ;   ;    ;    ;   ;  ;   ;  ;   ; 
  ;;; ;  ;;;    ; ;  ;;;  ;; ;;;;;;  ;;;    ;;; ;  ;;; ;
                                                        
                                                        
                                                        

      ;; download-known-docs : string -> void
      ;; downloads the docs to the tmp-dir
      (define (download-known-docs tmp-dir)
        (for-each (lambda (known-doc) (download-known-doc tmp-dir (car known-doc) (cdr known-doc)))
                  known-docs-plus-help))
      
      ;; download-known-doc : string string string -> void
      ;; stub is the `drscheme' portion of `drscheme-doc.plt'.
      (define (download-known-doc tmp-dir stub full-name)
        (let ([url (format doc-url-format stub)]
              [doc-name (make-local-doc-filename tmp-dir stub)])
          (show-message (format (string-constant refresh-downloading...) full-name))
          (call-with-output-file doc-name
            (lambda (out-port)
              (call/input-url (string->url url) get-pure-port 
                              (lambda (in-port)
                                (let loop ()
                                  (let ([s (read-string 1024 in-port)])
                                    (unless (eof-object? s)
                                      (display s out-port)
                                      (loop))))))))
          (void)))
      
      
                                          
    ;;         ;;;                        
     ;           ;            ;           
     ;           ;            ;           
  ;;;;   ;;;     ;     ;;;   ;;;;;   ;;;  
 ;   ;  ;   ;    ;    ;   ;   ;     ;   ; 
 ;   ;  ;;;;;    ;    ;;;;;   ;     ;;;;; 
 ;   ;  ;        ;    ;       ;     ;     
 ;   ;  ;   ;    ;    ;   ;   ;   ; ;   ; 
  ;;; ;  ;;;   ;;;;;;  ;;;     ;;;   ;;;  
                                          
                                          
                                      
      ;; delete-known-docs : -> void
      (define (delete-known-docs)
        (for-each (lambda (known-doc) (delete-known-doc (car known-doc) (cdr known-doc)))
                  known-docs-plus-help))
      
      (define (delete-known-doc doc full-name)
        (show-message (format (string-constant refresh-deleting...) full-name))
        (let ([doc-dir (build-path (collection-path "doc") doc)])
          (delete-directory/r doc-dir)))
      
      (define (delete-local-plt-files tmp-dir)
        (delete-directory/r tmp-dir))
      
      ;; delete-directory : string -> void
      ;; deletes the entire subtree underneath this directory
      ;; (including the dir itself)
      (define (delete-directory/r dir)
        (when (directory-exists? dir)
          (let loop ([dir dir])
            (let ([children (directory-list dir)])
              (for-each (lambda (f) (when (file-exists? (build-path dir f))
                                      (delete-file (build-path dir f))))
                        children)
              (for-each (lambda (d) (when (directory-exists? (build-path dir d))
                                      (loop (build-path dir d))))
                        children)
              (delete-directory dir)))))

      
                                                 
   ;                                ;;;    ;;;   
                       ;              ;      ;   
                       ;              ;      ;   
 ;;;   ; ;;;    ;;;   ;;;;;  ;;;;     ;      ;   
   ;    ;;  ;  ;   ;   ;         ;    ;      ;   
   ;    ;   ;   ;;;    ;      ;;;;    ;      ;   
   ;    ;   ;      ;   ;     ;   ;    ;      ;   
   ;    ;   ;  ;   ;   ;   ; ;   ;    ;      ;   
 ;;;;; ;;;  ;;  ;;;     ;;;   ;;; ; ;;;;;; ;;;;;;
                                                 
                                                 
    
    (define (install-known-docs tmp-dir)
      (with-installer-window
       (lambda (parent)
         (for-each (lambda (pr) 
                     (show-message (format (string-constant refresh-installing...) (cdr pr)))
                     (run-single-installer (make-local-doc-filename tmp-dir (car pr))
                                           parent))
                   known-docs-plus-help))))
    
      ;; go.
      (main))))
    