
;; Because Setup PLT is used to rebuild .zos, we may need to turn off
;; the use of compiled code or install cm before we do anything. This
;; startup stub parses the command line and either disables .zos or
;; installs cm for loading Setup PLT.

;; Note that this file is listed in "info.ss" so that it never gets a
;; .zo file. Do not `require' this module from anywhere, otherwise it
;; could get a .zo anyway.

(module setup mzscheme

  (define-values (flags specific-collections archives)
    ;; Load the command-line parser without using .zos, 
    ;;  and in its own namespace to avoid poluuting the cm-managed
    ;;  namespace later
    (parameterize ([use-compiled-file-kinds 'none]
		   [current-namespace (make-namespace)])
      ((dynamic-require '(lib "setup-cmdline.ss" "setup") 'parse-cmdline)
       (current-command-line-arguments))))

  (define (on? flag-name not)
    (let ([a (assq flag-name flags)])
      (and a (not (cadr a)))))

  (if (or (on? 'clean values)
	  (on? 'make-zo not))
      ;; Don't use .zos, in case they're out of date, and don't load
      ;;  cm:
      (use-compiled-file-kinds 'none)
  
      ;; Load the cm instance to be installed while loading Setup PLT.
      ;; This has to be dynamic, so we get a chance to turn off compiled
      ;; file loading, and so it can be in a separate namespace.
      (let-values ([(mk trust-zos)
		    (parameterize ([use-compiled-file-kinds 'none])
		      ;; Load cm.ss into its own namespace, so that cm compiles
		      ;; itself and its required modules in the right order
		      ;; (i.e., when some module requires cm or one of its
		      ;; required modules)
		      (parameterize ([current-namespace (make-namespace)])
			(values
			 (dynamic-require '(lib "cm.ss") 
					  'make-compilation-manager-load/use-compiled-handler)
			 (dynamic-require '(lib "cm.ss") 'trust-existing-zos))))])
	(when (on? 'trust-existing-zos values)
	  (trust-zos #t))
	(current-load/use-compiled (mk))))
  
  ;; This has to be dynamic, so we get a chance to turn off
  ;; .zo use and turn on the compilation manager.
  (dynamic-require '(lib "setup-go.ss" "setup") #f))
