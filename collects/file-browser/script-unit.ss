(module script-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           "sigs.ss")

  (define-syntax check-arg
    (syntax-rules ()
      ((_ arg pred? name-symbol expected-string bad-k v ...)
       (if (not (pred? arg))
           (raise-type-error name-symbol expected-string bad-k v ...)))))
  
  (provide script@)
  
  (define script@
    (unit/sig script^
      (import (gui : gui^) (fs : file-system^))
  
      (define-syntax file-arg!
        (syntax-rules ()
          ((_ arg name-symbol bad-k v ...)
           (cond
             ((string? arg)
              (set! arg (fs:make-file-from-path arg)))
             ((not (fs:file? arg))
              (raise-type-error name-symbol "file or string" bad-k v ...))))))
 
      ;; get-current-dir: -> file
      (define get-current-dir fs:get-current-dir)
      
      ;; set-current-dir!: (file U string) ->
      (define (set-current-dir! file)
        (file-arg! file 'set-current-dir! 0 file)
        (fs:set-current-dir! file))
      
      ;; make-file: (file U string) -> file
      (define (make-file file)
        (file-arg! file 'make-file 0 file)
        file)
        
      ;; file-name: (file U string) -> string
      (define (file-name file)
        (file-arg! file 'file-name 0 file)
        (fs:file-name file))
      
      ;; file-dir: (file U string) -> file
      (define (file-dir file)
        (file-arg! file 'file-dir 0 file)
        (fs:make-file-from-path (fs:file-dir file)))
      
      ;; file-full-path: (file U string) -> string
      (define (file-full-path file)
        (file-arg! file 'file-full-path 0 file)
        (fs:file-full-path file))
      
      ;; file: 'a -> bool
      (define file? fs:file?)
      
      ;; file=?: (file U string) * (file * string) -> bool
      (define (file=? f1 f2)
        (file-arg! f1 'file=? 0 f1 f2)
        (file-arg! f2 'file=? 1 f1 f2)
        (fs:file=? f1 f2))
      
      ;; selection: file list
      (define selection null)
      
      ;; print-selection: ->
      (define (print-selection)
        (parameterize ((current-inspector fs:file-inspector))
          (pretty-print selection)))
      
      ;; clear-selection: ->
      (define (clear-selection)
        (set! selection null)
        (gui:selection-updated))
      
      ;; cons-selection: (file U string) ->
      (define (cons-selection f)
        (file-arg! f 'cons-selection 0 f)
        (set! selection (cons f selection))
        (gui:selection-added f))
      
      ;; internal-remove-selection: file ->
      (define (internal-remove-selection f)
        (let ((found #f))
          (set! selection
                (let loop ((s selection))
                  (cond
                    ((null? s) null)
                    ((fs:file=? f (car s)) 
                     (set! found #t)
                     (loop (cdr s)))
                    (else (cons (car s) (loop (cdr s)))))))
          found))
              
      ;; remove-selection: (file U string) ->
      (define (remove-selection f)
        (file-arg! f 'remove-selection 0 f)
        (if (internal-remove-selection f)
            (gui:selection-removed f)))
           
      ;; filter-selection: (file -> bool) ->
      (define (filter-selection f)
        (check-arg f procedure? 'filter-selection "procedure" 0 f)
        (set! selection (filter f selection))
        (gui:selection-updated))
      
      ;; map-selection: (file -> 'a) -> 'a list
      (define (map-selection f)
        (check-arg f procedure? 'map-selection "procedure" 0 f)
        (map f selection))
      
      ;; directory-list: (file U string) -> file list
      (define (directory-list dir)
        (file-arg! dir 'directory-list 0 dir)
        (fs:directory-list dir))
      
      ;; is-directory?: (file U string) -> bool
      (define (is-directory? f)
        (file-arg! f 'is-directory? 0 f)
        (fs:is-directory? f))
      
      ;; delete-file: (file U string) ->
      (define (delete-file f)
        (file-arg! f 'delete-file 0 f)
        (let ((was-selected (internal-remove-selection f)))
          (with-handlers ((exn:i/o:filesystem? (lambda (ex)
                                                 (if was-selected
                                                     (set! selection (cons f selection)))
                                                 (raise ex))))
            (fs:delete-file f))))
        
      ;; new-file: string * (file U string) -> file
      (define (new-file name dir)
        (check-arg name string? 'new-file "string" 0 name dir)
        (file-arg! dir 'new-file 1 name dir)
        (fs:new-file name dir #f))

      ;; new-directory: string * (file U string) -> file
      (define (new-directory name dir)
        (check-arg name string? 'new-directory "string" 0 name dir)
        (file-arg! dir 'new-directory 1 name dir)
        (fs:new-file name dir #t))

      ;; rename-file: (file U string) * string -> file
      (define (rename-file file new-name)
        (file-arg! file 'rename-file 0 file new-name)
        (check-arg new-name string? 'rename-file "string" 1 file new-name)
        (let ((was-selected (internal-remove-selection file)))
          (with-handlers ((exn:i/o:filesystem? (lambda (ex)
                                                 (if was-selected
                                                     (set! selection (cons file selection)))
                                                 (raise ex))))
            (let ((new-file (fs:rename-file file new-name)))
              (cons-selection new-file)
              new-file))))
        
      ;; move-file: (file U string) (file U string) -> file
      (define (move-file file new-dir)
        (file-arg! file 'move-file 0 file new-dir)
        (file-arg! new-dir 'move-file 1 file new-dir)
        (let ((was-selected (internal-remove-selection file)))
          (with-handlers ((exn:i/o:filesystem? (lambda (ex)
                                                 (if was-selected
                                                     (set! selection (cons file selection)))
                                                 (raise ex))))
            (let ((new-file (fs:move-file file new-dir)))
              (cons-selection new-file)
              new-file))))
      
      ;; open-dir-window: (file U string) ->
      (define (open-dir-window dir)
        (file-arg! dir 'open-dir 0 dir)
        (gui:add-window dir))
      
      ;; close-dir-window: ->
      (define (close-dir-window)
        (gui:close-window)))))
      
