
(define graphical-debug? (getenv "GRAPHICAL_DEBUG"))
(define textual-debug? #f)

(require-library "macro.ss")
(when graphical-debug?
  (require-library "grepl.ss" "drscheme"))
(when (or graphical-debug? textual-debug?)
  (require-library "rload.ss" "drscheme")
  (require-library "errortrace.ss" "errortrace"))

(require-library "mred-interfaces.ss" "framework")

(error-print-width 80)

(require-library "refer.ss")

(define original-output-port (current-output-port))
(define (orig-output t)
  (parameterize ([current-output-port original-output-port])
    (t)))

(define drscheme-namespace (current-namespace))
(define drscheme-custodian #f)
(define drscheme-eventspace #f)

(load-relative "start-drs.ss")

(define (u)
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian]
		 [current-namespace drscheme-namespace])
    (set! drscheme-eventspace (make-eventspace))
    (parameterize ([current-eventspace drscheme-eventspace])
      (when (or graphical-debug? textual-debug?)
        (queue-callback 
         (lambda ()
           (invoke-unit (require-library "errortracer.ss" "errortrace")))))
      (queue-callback start-drscheme))))

(cond
  [graphical-debug?
   (graphical-read-eval-print-loop)]
  [textual-debug?
   (require-library "rep.ss" "readline")
   (read-eval-print-loop)]
  [else
   (u)
   (yield (make-semaphore 0))])
