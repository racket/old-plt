
;; Because setup is used to rebuild .zos, we may need to turn off the
;; use of compiled code before we do anything. This startup stub looks
;; at argv and pessimistically turns of compiled files if -c could be
;; in the command line.

(module setup mzscheme
  
  (for-each (lambda (i)
	      (when (regexp-match "^-.*c" i)
		(use-compiled-file-kinds 'none)))
	    (vector->list (current-command-line-arguments)))
  
  ;; This has to be dynamic, so we get a chance to turn off
  ;; compiled file loading.
  (let ([mk
	 (parameterize ([use-compiled-file-kinds 'none])
           ;; Load cm.ss into its own namespace, so that cm compiles itself later:
           (parameterize ([current-namespace (make-namespace)])
             (dynamic-require '(lib "cm.ss") 
                              'make-compilation-manager-load/use-compiled-handler)))])
    (current-load/use-compiled (mk)))
  
  ;; This has to be dynamic, so we get a chance to turn on
  ;; the compilation manager.
  (dynamic-require '(lib "setup-go.ss" "setup") #f))
