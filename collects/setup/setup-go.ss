
;; Because setup is used to rebuild .zos, we may need to turn off the
;; use of compiled code before we do anything. This startup stub looks
;; at argv and pessimistically turns of compiled files if -n could be
;; in the command line.

(module setup-go mzscheme
  
  (for-each (lambda (i)
	      (when (regexp-match "^-.*c" i)
		(use-compiled-file-kinds 'none)))
	    (vector->list (global-defined-value 'argv)))

  ;; This has to be dynamic, so we get a chance to turn off
  ;; compiled file loading.
  (dynamic-import '(lib "setup.ss" "setup") #f))
