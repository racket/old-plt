
;; Because setup is used to rebuild .zos, we may need to turn off the
;; use of compiled code before we do anything. This startup stub looks
;; at argv and pessimistically turns of compiled files if -n could be
;; in the command line.

(module setup mzscheme
  
  (for-each (lambda (i)
	      (when (regexp-match "^-.*c" i)
		(use-compiled-file-kinds 'none)))
	    (vector->list (namespace-variable-binding 'argv)))

  ;; This has to be dynamic, so we get a chance to turn off
  ;; compiled file loading.
  (dynamic-require '(lib "setup-go.ss" "setup") #f))
