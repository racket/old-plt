(define graphical-debug? (getenv "GRAPHICAL_DEBUG"))
(define textual-debug? #f)

(require-library "macro.ss")
(when graphical-debug?
  (require-library "grepl.ss" "drscheme")
  (memory-counter))
(when (or graphical-debug? textual-debug?)
  (require-library "rload.ss" "drscheme")
  (require-library "errortrace.ss" "errortrace"))

(error-print-width 80)

(require-library "refer.ss")

(define original-output-port (current-output-port))
(define (orig-output t)
  (parameterize ([current-output-port original-output-port])
    (t)))

(define drscheme-namespace #f)
(define drscheme-custodian #f)
(define drscheme-eventspace #f)

(load-relative "start-drs.ss")

(define (u)
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian])
    (set! drscheme-eventspace (make-eventspace))
    (set! drscheme-namespace (make-namespace))
    (when (or graphical-debug? textual-debug?)
      (parameterize ([current-eventspace drscheme-eventspace])
        (queue-callback 
         (lambda ()
           [current-namespace drscheme-namespace]
           [current-eventspace drscheme-eventspace]
           (exit-handler (lambda ___ (custodian-shutdown-all drscheme-custodian)))
           (invoke-unit (require-library "errortracer.ss" "errortrace"))))))
    (queue-callback start-drscheme)))

(cond
  [graphical-debug?
   (graphical-read-eval-print-loop)]
  [textual-debug?
   (require-library "rep.ss" "readline")
   (read-eval-print-loop)]
  [else
   (u)
   (yield (make-semaphore 0))])
