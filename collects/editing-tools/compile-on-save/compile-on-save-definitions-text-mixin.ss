(module compile-on-save-definitions-text-mixin mzscheme
  (provide compile-on-save-definitions-text-mixin)
  (require (lib "compiler.ss" "compiler")
           (lib "cm.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "file.ss")
           (lib "etc.ss")
           
           "../common/drscheme-tool-support.ss"
           
           )

  (define cos<%> (interface ()))
  
  (define (compile-on-save-definitions-text-mixin dt%)
    (if (implementation? dt% cos<%>)
        dt%
    (class* (shared-mixin dt%) (cos<%>)
      
      (super-new)
      ;; from DrScheme's editor
      (inherit get-filename)
      ;; from the shared-mixin
      (inherit make-syntax-error-handler kill-autocompile-thread set-autocompile-thread-proc! clear-error-canvas)
      
      (define/augment (after-save-file success?)
        (when success?
          (kill-autocompile-thread)
          (let ([filename (get-filename)])
            (set-autocompile-thread-proc! (lambda () (autocompile filename))))))

      (define/private (autocompile filename)
        (with-handlers ([exn:fail:syntax? (make-syntax-error-handler filename)]
                        [void (lambda (e)
                                (warn 'on-save-file
                                      "Could not compile ~a.  Reason in exn ~a: ~a"
                                      filename e (exn-message e)))])
          (parameterize ([manager-trace-handler append-status]
                         [manager-compile-notify-handler (lambda (file)
                                                           (append-status (format "COMPILING: ~a"
                                                                                  file)))])
            ((make-caching-managed-compile-zo) filename)))
        ;; successful compile, clear error indicators
        (clear-error-canvas))
      
      )))
  
  #| In case we want to skip CM, try this:
  (define build-zo
    (let ([bz (compile-zos #f)])
      (lambda (filename)
        (bz (cons filename null) 'auto))))
  |#
  
  )
