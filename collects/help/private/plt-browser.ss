(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?)

  (define (help-browser-preference)
    (get-preference 'plt:help-browser
		    (lambda () 'external)))

  ;; use-plt-browser? : -> boolean
  ;; returns #t if the browser preference is 'plt
  ;; and we aren't running in standalone mode
  (define (use-plt-browser?)
    (let* ([program (with-handlers ([not-break-exn? (lambda (x) #f)])
                      (namespace-variable-value 'program))]
           [in-mz?
            (and (string? program)
                 (regexp-match "mzscheme" program))])
      (and (not in-mz?)
           (eq? (help-browser-preference) 'plt)))))



