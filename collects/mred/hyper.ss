
(load "hypersig.ss")

(load "hypredit.ss")
(load "hyprfram.ss")
(load "hyprdial.ss")

#|

(define-sigfunctor (mred:hyper-loader@ mred:hyper-loader~)
  (import mred:edit~ mred:frame~ mred:canvas~ mred:group~
	  mred:handler~)

  (mred:handler~:insert-format-handler  "Hyper-Text" "htx" 
					(lambda (filename group)
					  (open-hyper-make filename group)))

  (define loaded? #f)
  (define real-open-make #f)
  (define real-open-view #f)

  (define open-hyper-make
    (lambda args
      (hyper-text-require)
      (apply real-open-make args)))
      
  (define open-hyper-view
    (lambda args
      (hyper-text-require)
      (apply real-open-make args)))
      
  (define hyper-text-require
    (lambda ()
      (when (not loaded?)
	    (let ([dir (build-path (global-defined-value 
				    mred:system-source-directory) 
				   "hyper")])
	      (load (build-path dir "hypersig.ss"))
	      (load (build-path dir "hypredit.ss"))
	      (load (build-path dir "hyprdial.ss"))
	      (load (build-path dir "hyprfram.ss"))
	      
	      link [& open] somehow
	       

  )

|#
