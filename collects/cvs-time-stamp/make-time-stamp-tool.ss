(module make-time-stamp-tool mzscheme

  (define (month->sym n)
    (case n
      [(1) 'jan]
      [(2) 'feb]
      [(3) 'mar]
      [(4) 'apr]
      [(5) 'may]
      [(6) 'jun]
      [(7) 'jul]
      [(8) 'aug]
      [(9) 'sep]
      [(10) 'oct]
      [(11) 'nov]
      [(12) 'dec]))

  (define current-date (seconds->date (current-seconds)))
  (define date-stamp (format "~a~a~a"
                             (date-day current-date)
                             (month->sym (date-month current-date))
                             (date-year current-date)))
  

  (call-with-output-file (build-path (collection-path "cvs-time-stamp") "time-stamp.ss")
    (lambda (port)
      (write
       `(module time-stamp mzscheme
          (require (lib "tool.ss" "drscheme")
                   (lib "unitsig.ss")
                   (lib "framework.ss" "framework"))
          
          (provide tool@)
          
          (define tool@
            (unit/sig ()
              (import drscheme:tool^)
              (version:add-spec '-cvs ,date-stamp))))
       port))
    'text 'truncate))