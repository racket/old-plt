(module contract-helpers mzscheme
  (require (lib "file.ss")
           (lib "moddep.ss" "syntax"))

  (provide module-source-as-symbol)
  
  ;; module-source-as-symbol : syntax -> symbol
  ;; constructs a symbol for use in the blame error messages
  ;; when blaming the module where stx's occurs.
  (define (module-source-as-symbol stx)
    (let ([src-module (syntax-source-module stx)])
      (cond
        [(symbol? src-module) src-module]
        [(module-path-index? src-module) 
         (string->symbol 
          (format 
           "~a" 
           (resolve-module-path-index src-module #f)))]
        [else 'top-level])))
  
#|

a module-path-index is either
  - (module-path-index-join module-path module-path-index)
  - #f

a module-path is either:
  - top-level-module-path
  - absolute-module-path
  - relative-module-path
  
a top-level-module-path is a symbol
a relative-module-path is a string
an absolute-module-path is either:
  - `(lib ,string ...)
  - `(file ,string)
  
|#

  ;; module-path-index->filename : module-path-index -> (union #f string)
  ;; returns a string containing the file corresponding to the
  ;; input module path index, or #f if the file can't be found.
  (define (module-path-index->filename mpi)
    (let/ec k
      (let loop ([mpi mpi])
        (cond
          [(module-path-index? mpi)
           (let-values ([(module-path sub-mpi) (module-path-index-split mpi)])
             (cond
               [(absolute-module-path? module-path)
                (interpret-absolute-module-path module-path)]
               [(relative-module-path? module-path)
                (if sub-mpi
                    (build-path (path-only (loop sub-mpi)) module-path)
                    (k #f))]
               [(top-level-module-path? module-path) (loop sub-mpi)]))]

          ;; is this case necessary?
          [else (k #f)]))))
  
  ;; top-level-module-path? : module-path -> boolean
  (define (top-level-module-path? module-path)
    (symbol? module-path))

  ;; absolute-module-path? : module-path -> boolean
  (define (absolute-module-path? module-path)
    (and (pair? module-path)
         (or (eq? (car module-path) 'file)
             (eq? (car module-path) 'lib))))

  
  ;; relative-module-path? : module-path -> boolean
  (define (relative-module-path? module-path)
    (string? module-path))

  ;; interpret-absolute-module-path : absolute-module-path -> string
  (define (interpret-absolute-module-path module-path)
    (let ([comma-d (symbol->string ((current-module-name-resolver) module-path #f #f))])
      (substring comma-d 1 (string-length comma-d)))))
