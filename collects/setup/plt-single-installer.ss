(module plt-single-installer mzscheme 
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           
	   ;; All the rest are to get the imports for setup@:
	   "option-sig.ss"
	   "setup-unit.ss"
	   "option-unit.ss"
	   (lib "launcher-sig.ss" "launcher")
	   (lib "launcher-unit.ss" "launcher")
	   (lib "dynext-sig.ss" "dynext")
	   (lib "dynext-unit.ss" "dynext")
	   (lib "sig.ss" "compiler")
	   (lib "option-unit.ss" "compiler")
	   (lib "compiler-unit.ss" "compiler"))

  (provide run-single-installer)

  ;; run-single-installer : string (-> string) -> void
  ;; creates a separate thread, runs the installer in that thread,
  ;; returns when the thread completes
  (define (run-single-installer file get-target-dir)
    (let ([cust (make-custodian)])
      (parameterize ([current-custodian cust]
		     [current-namespace (make-namespace)]
		     [exit-handler (lambda (v) (custodian-shutdown-all cust))])
	(let ([thd
	       (thread
		(lambda ()
		  (invoke-unit/sig
		   (compound-unit/sig
		    (import)
		    (link [launcher : launcher^ (launcher@ dcompile dlink)]
			  [dcompile : dynext:compile^ (dynext:compile@)]
			  [dlink : dynext:link^ (dynext:link@)]
			  [dfile : dynext:file^ (dynext:file@)]
			  [option : compiler:option^ (compiler:option@)]
			  [compiler : compiler^ (compiler@
						 option
						 dcompile
						 dlink
						 dfile)]
			  [soption : setup-option^ (setup:option@)]
			  [set-options : () ((unit/sig ()
					       (import setup-option^ compiler^)
					       ;; >>>>>>>>>>>>>> <<<<<<<<<<<<<<<
					       ;; Here's where we tell setup the archive file!
					       (archives (list file))
					       ;; Here's where we make get a directory:
					       (current-target-directory-getter
						get-target-dir))
					     soption
					     compiler)]
			  [setup : () (setup@
				       SOPTION
				       compiler
				       option
				       launcher)])
		    (export)))))])
	  (thread-wait thd)
	  (custodian-shutdown-all cust))))))

