(module refresh-manuals mzscheme
  (require "private/docpos.ss"
           "private/search.ss"
           "private/manuals.ss"
           "private/standard-urls.ss"
           (lib "plt-installer.ss" "setup")
           (lib "url.ss" "net")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           (lib "contract.ss")
	   (lib "thread.ss"))
  
  (define doc-prs (listof (cons/p string? string?)))
  
  (provide refresh-manuals)
  
  (define sc-refreshing-manuals (string-constant plt:hd:refreshing-manuals))
  (define sc-refresh-downloading... (string-constant plt:hd:refresh-downloading...))
  (define sc-refresh-deleting... (string-constant plt:hd:refresh-deleting...))
  (define sc-refresh-installing... (string-constant plt:hd:refresh-installing...))
  (define sc-finished-installation (string-constant plt:hd:refreshing-manuals-finished))
  (define sc-clearing-cached-indicies (string-constant plt:hd:refresh-clearing-indicies))
  
  (define refresh-manuals
    (case-lambda
      [() (refresh-manuals known-docs)]
      [(docs-to-install)
       (let ([tmp-directory (find/create-temporary-docs-dir)]
             [success? #f]
             [thd #f])
         (with-installer-window
          (lambda (parent)
            (set! thd (current-thread))
            (unless tmp-directory
              (error 'plt-installer "please clean out ~a" (find-system-path 'temp-dir)))
            (download-docs docs-to-install tmp-directory)
            (delete-docs docs-to-install)
            (install-docs docs-to-install tmp-directory parent)
            (delete-local-plt-files tmp-directory)
            (display sc-clearing-cached-indicies)
            (newline)
            (doc-collections-changed)
            (display sc-finished-installation)
            (newline)
            (set! success? #t))
          (lambda ()
            (unless success?
              (delete-local-plt-files tmp-directory))
            (kill-thread thd))))]))
      
  (define (make-local-doc-filename tmp-dir stub)
    (build-path tmp-dir (format "~a-doc.plt" stub)))

  ;; if cannot find a suitable directory, #f is returned
  ;; if okay, returns the path to the directory.
  (define find/create-temporary-docs-dir
    ;(-> (union string? false?))
    (lambda ()
      (let ([temp-dir (find-system-path 'temp-dir)])
        (let loop ([n 0])
          (if (= n 30)
              #f
              (let ([candidate (build-path temp-dir (format "help-refresh-docs~a" n))])
                (if (directory-exists? candidate)
                    (loop (+ n 1))
                    (begin
                      (make-directory candidate)
                      candidate))))))))
                  

      


                                                        
    ;;                       ;;;                     ;; 
     ;                         ;                      ; 
     ;                         ;                      ; 
  ;;;;   ;;;  ;;; ;;;; ;;;     ;     ;;;   ;;;;    ;;;; 
 ;   ;  ;   ;  ;   ;  ;;  ;    ;    ;   ;      ;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;   ;;;;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
 ;   ;  ;   ;   ; ;   ;   ;    ;    ;   ;  ;   ;  ;   ; 
  ;;; ;  ;;;    ; ;  ;;;  ;; ;;;;;;  ;;;    ;;; ;  ;;; ;
                                                        
                                                        
                                                        

  ;; downloads the docs to the tmp-dir
  (define/contract download-docs
    (doc-prs string? . -> . any)
    (lambda (docs-to-install tmp-dir)
      (for-each (lambda (known-doc) (download-doc tmp-dir (car known-doc) (cdr known-doc)))
                docs-to-install)))
      
  ;; stub is the `drscheme' portion of `drscheme-doc.plt'.
  (define/contract download-doc
    (string? string? string? . -> . any)
    (lambda (tmp-dir stub full-name)
      (let ([url (make-docs-plt-url stub)]
            [doc-name (make-local-doc-filename tmp-dir stub)])
        (display (format sc-refresh-downloading... full-name))
        (newline)
        (call-with-output-file doc-name
          (lambda (out-port)
            (call/input-url (string->url url) get-pure-port 
                            (lambda (in-port)
			      (copy-port in-port out-port)))))
        (void))))
      
      
                                          
    ;;         ;;;                        
     ;           ;            ;           
     ;           ;            ;           
  ;;;;   ;;;     ;     ;;;   ;;;;;   ;;;  
 ;   ;  ;   ;    ;    ;   ;   ;     ;   ; 
 ;   ;  ;;;;;    ;    ;;;;;   ;     ;;;;; 
 ;   ;  ;        ;    ;       ;     ;     
 ;   ;  ;   ;    ;    ;   ;   ;   ; ;   ; 
  ;;; ;  ;;;   ;;;;;;  ;;;     ;;;   ;;;  
                                          
                                          
                                      
  (define/contract delete-docs
    (doc-prs . -> . any)
    (lambda (docs)
      (for-each (lambda (known-doc) (delete-known-doc (car known-doc) (cdr known-doc)))
                docs)))
      
  (define/contract delete-known-doc
    (string? string? . -> . any)
    (lambda (doc full-name)
      (let ([doc-dir (find-doc-directory doc)])
        (when doc-dir
          (display (format sc-refresh-deleting... full-name))
          (newline)
          (delete-directory/r doc-dir)))))
      
  (define/contract delete-local-plt-files
    (string? . -> . any)
    (lambda (tmp-dir)
      (delete-directory/r tmp-dir)))
      
  ;; deletes the entire subtree underneath this directory
  ;; (including the dir itself)
  (define/contract delete-directory/r 
    (string? . -> . any)
    (lambda (dir)
      (when (directory-exists? dir)
        (let loop ([dir dir])
          (let ([children (directory-list dir)])
            (for-each (lambda (f) (when (file-exists? (build-path dir f))
                                    (delete-file (build-path dir f))))
                      children)
            (for-each (lambda (d) (when (directory-exists? (build-path dir d))
                                    (loop (build-path dir d))))
                      children)
            (delete-directory dir))))))

      
                                                 
   ;                                ;;;    ;;;   
                       ;              ;      ;   
                       ;              ;      ;   
 ;;;   ; ;;;    ;;;   ;;;;;  ;;;;     ;      ;   
   ;    ;;  ;  ;   ;   ;         ;    ;      ;   
   ;    ;   ;   ;;;    ;      ;;;;    ;      ;   
   ;    ;   ;      ;   ;     ;   ;    ;      ;   
   ;    ;   ;  ;   ;   ;   ; ;   ;    ;      ;   
 ;;;;; ;;;  ;;  ;;;     ;;;   ;;; ; ;;;;;; ;;;;;;
                                                 
                                                 
    
  (define/contract install-docs
    (doc-prs string? (is-a?/c top-level-window<%>) . -> . any)
    (lambda (docs-to-install tmp-dir parent)
      (for-each (lambda (pr) 
                  (display (format sc-refresh-installing... (cdr pr)))
                  (newline)
                  (run-single-installer (make-local-doc-filename tmp-dir (car pr))
                                        parent))
                docs-to-install))))
