(module file-system "mzrestricted.ss"
  (require (lib "class.ss")
           (prefix file: (lib "file.ss"))
           (lib "unitsig.ss")
           "sigs.ss"
           (prefix mz: mzscheme)
           "weak-set.ss")
  
  (provide file-system-state% make-file-system@)
  
  (define-local-member-name notify-all)
  
  (define file-system-state%
    (class object%
      (super-instantiate ())
      (define dependants (make-weak-set))
      
      (define/public (register-viewport file-window)
        (weak-set-add! file-window dependants))
      
      (define/public (notify-all-add file)
        (weak-set-for-each! (lambda (win) (send win file-added file)) dependants))

      (define/public (notify-all-delete file)
        (weak-set-for-each! (lambda (win) (send win file-deleted file)) dependants))))
  
  (define (make-file-system@ state)
    (unit/sig file-system^
      (import (gui : gui^))
      
      ;; (make-file string string string)
      ;; A file will always represent an absolute path
      (define file-inspector (current-inspector))
      (define-struct file (name dir full-path) (make-inspector))
      
      ;; make-file-from-dir-name: string * string -> file
      (define (make-file-from-name-dir name dir)
        (let ((full-path
               (mz:simplify-path
                (cond
                  ((mz:relative-path? dir) (mz:build-path (file-full-path 
                                                           (gui:get-current-directory))
                                                          dir name))
                  (else (mz:build-path dir name))))))
          (make-file (string->immutable-string name)
                     (string->immutable-string dir)
                     (string->immutable-string full-path))))

      ;; make-file-from-path: string -> path
      (define (make-file-from-path path)
        (let ((full-path
               (mz:simplify-path
                (cond
                  ((mz:relative-path? path) (mz:build-path (file-full-path 
                                                            (gui:get-current-directory))
                                                           path))
                  (else (mz:build-path path))))))
          (let-values (((base name _) (mz:split-path full-path)))
            (cond
              (base
               (make-file (string->immutable-string name)
                          (string->immutable-string base)
                          (string->immutable-string full-path)))
              (else
               (make-file "" "/" "/"))))))
      
      ;; path=?: string * string -> bool
      (define (path=? p1 p2)
        (string=? (mz:normal-case-path (file:normalize-path p1))
                  (mz:normal-case-path (file:normalize-path p2))))
      
      ;; file=?: file * file -> bool
      (define (file=? f1 f2)
        (path=? (file-full-path f1) (file-full-path f2)))
      
      ;; is-directory: file -> bool
      (define (is-directory? file)
        (mz:directory-exists? (file-full-path file)))
      
      ;; directory-list: file -> file list
      (define (directory-list dir)
        (map (lambda (name) (make-file-from-name-dir name (file-full-path dir)))
             (mz:directory-list (file-full-path dir))))
      
      ;; new-file: string * file * bool -> file
      (define (new-file name dir is-dir?)
        (let ((file (make-file-from-name-dir name (file-full-path dir))))
          (cond
            (is-dir? (mz:make-directory (file-full-path file)))
            (else
             (let ((x (open-output-file (file-full-path file))))
               (close-output-port x))))
          (send state notify-all-add file)
          file))
      
      ;; rename-file: file * string -> file
      (define (rename-file file new-name)
        (let ((new-file (make-file-from-name-dir new-name (file-full-path file))))
          (mz:rename-file-or-directory (file-full-path file) (file-full-path new-file))
          (send state notify-all-add new-file)
          (send state notify-all-delete file)
          new-file))
      
      ;; move-file: file * file -> file
      (define (move-file file new-dir)
        (let ((new-file (make-file-from-name-dir (file-name file) 
                                                 (file-full-path new-dir))))
          (mz:rename-file-or-directory (file-full-path file) (file-full-path new-file))
          (send state notify-all-add new-file)
          (send state notify-all-delete file)
          new-file))
      
      ;; delete-file: file ->
      (define (delete-file file)
        (mz:delete-file (file-full-path file))
        (send state notify-all-delete file)))))
