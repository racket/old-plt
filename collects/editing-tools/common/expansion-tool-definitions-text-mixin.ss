(module expansion-tool-definitions-text-mixin mzscheme
   (provide expansion-tool-definitions-text-mixin expansion-tool<%>)
  (require (lib "compiler.ss" "compiler")
           (lib "cm.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "file.ss")
           (lib "etc.ss")
           
           "drscheme-tool-support.ss"
           
           )
  
  (define expansion-tool<%>
    (interface () #|latest-expansion|# buffer-directory))
  
  (define (expansion-tool-definitions-text-mixin dt%)
    (if (implementation? dt% expansion-tool<%>)
        dt%
        (class* dt% (expansion-tool<%>)
          (super-new)
          
          (inherit get-filename)
          
          ;; FIXME: should be protected
          (field [latest-expansion null]);#'(module initial-latest-expansion mzscheme (#%plain-module-begin)))
          
          ;; FIXME: should be protected
          (define/public (buffer-directory)
            (define filename (get-filename))
            (when-debugging (printf "buffer-directory: filename is ~v~n" filename))
            (if filename
                (let-values ([(base path dir?)
                              (split-path filename)])
                  base)
                (current-directory)))
          )))
    
  )
