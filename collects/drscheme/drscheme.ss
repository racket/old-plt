(require-library "macro.ss")
(require-library "grepl.ss" "drscheme")
(require-library "rload.ss" "drscheme")

(require-library "errortrace.ss" "errortrace")

(error-print-width 80)

(require-library "refer.ss")

(define graphical-debug? #t)
(define textual-debug? #f)

(define drscheme-namespace (current-namespace))
(define drscheme-custodian #f)
(define drscheme-eventspace #f)

(load-relative "start-drs.ss")

(define (help-desk)
  (require-library "core.ss")
  (require-library "url.ss" "net")
  (require-library "sig.ss" "help")
  (eval
   '(invoke-open-unit/sig
     (require-library "start-help-desk.ss" "help")
     #f
     mzlib:function^
     mzlib:string^
     mzlib:file^
     mzlib:url^
     mred^))
  (thread
   (lambda ()
     (parameterize ([current-eventspace (make-eventspace)])
       (start-help-desk)))))

(define (U)
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian]
		 [current-namespace drscheme-namespace])
    (set! drscheme-eventspace (make-eventspace))
    (parameterize ([current-eventspace drscheme-eventspace])
      (start-drscheme))))

(cond
  [graphical-debug?
   (graphical-read-eval-print-loop)]
  [textual-debug?
   (require-library "rep.ss" "readline")
   (read-eval-print-loop)]
  [else
   (U)
   (yield (make-semaphore 0))])