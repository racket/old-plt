(module make mzscheme
  (require (lib "launcher.ss" "launcher"))

  (make-mred-launcher '("-e"
			"(use-compiled-file-kinds 'none)"
			"-mvqM"
			"finish-install")
		      (mred-program-launcher-path "Finish Install")
		      (cons
		       '(forget-exe? . #t)
		       (build-aux-from-path (build-path
					     (collection-path "finish-install")
					     "finish-install")))))
