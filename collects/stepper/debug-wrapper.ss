(unit/sig plt:aries^
  (import [zodiac : zodiac:system^]
          [mred : mred^]
          [utils : stepper:cogen-utils^]
          [marks : stepper:marks^]
          [annotate : stepper:annotate^])
  
  (define w-c-m-key annotate:debug-key)
  
  (define current-environments #f)
  
  (define drscheme-eventspace (mred:current-eventspace))
  (define break-semaphore (make-semaphore))
  (define break-resume-value #f)
  
  (define (annotate sexp zodiac-read)
    (let-values 
        ([(annotateds new-envs)
          (annotate:annotate (and zodiac-read (list zodiac-read)) 
                             (list sexp) 
                             current-environments 
                             #f)])
      (set! current-environments new-envs)
      (car annotateds)))
  
  (define (extract-zodiac-location mark-set)
    (let ([mark-list (continuation-mark-set->list mark-set annotate:debug-key)])
      (if (null? mark-list)
          #f
          (marks:mark-source (car mark-list)))))
  
  (define (make-zodiac-mark location)
    (marks:make-mark location #f null))
    
  (define (break)
    (let ([break-info (continuation-mark-set->list (current-continuation-marks) annotate:debug-key)])
      (parameterize
          ([mred:current-eventspace drscheme-eventspace])
        (mred:queue-callback 
         (lambda ()
           (current-namespace (make-namespace))
           (global-defined-value 'break-info break-info)
           (global-defined-value 'break-resume (lambda (val) 
                                                 (set! break-resume-value val)
                                                 (semaphore-post break-semaphore)))
           (global-defined-value 'expose-mark marks:expose-mark)
           (global-defined-value 'display-mark marks:display-mark)
           (mred:graphical-read-eval-print-loop)))
        (semaphore-wait break-semaphore)
        break-resume-value)))
  
  (define signal-not-boolean utils:signal-not-boolean)
  (define signal-undefined utils:signal-undefined)
  
  ; initialization --- should be called once per execute
  ; (except that (2000-02-20) it doesn't matter anyway because
  ; these environments are totally irrelevant to non-stepper
  ; use of the annotater.
  (set! current-environments annotate:initial-env-package))

  