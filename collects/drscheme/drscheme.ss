(require-library "grepl.ss" "drscheme")
(require-library "rrequire.ss" "drscheme")
(require-library "errortrace.ss" "errortrace")

(error-print-width 80)

(require-library "refer.ss")

(define graphical-debug? #t)

(define drscheme-custodian #f)
(define drscheme-eventspace #f)

(load-relative "start-drs.ss")

(define run-test void)

(define (T)
  (set! run-test (lambda x (error 'run-test "not defined")))
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian])
    (set! drscheme-eventspace (make-eventspace))
    (parameterize ([current-eventspace drscheme-eventspace])
      (start-drscheme)))
  (set! run-test (load (build-path (collection-path "tests" "drscheme") "run-test.ss")))
  (send loading-message set-label ""))

(define start-drscheme-expression '(T))

(cond
  [graphical-debug?
   (graphical-read-eval-print-loop)]
  [else
   (require-library "rep.ss" "readline")
   (read-eval-print-loop)])