
(module load-handlers mzscheme
  (require (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "zodiac.ss" "syntax")
           (lib "basis.ss" "userspce"))
  
  (provide load-handler@)

  (define load-handler@
    (unit/sig drscheme:load-handler^
      (import)
      
      (define (process-text/zodiac text f start end annotate? text-is-file?)
        (let ([setting (basis:current-setting)]
              [file (if text-is-file?
                        text
                        (or (send text get-filename)
                            "Unknown"))]
              [init-line (+ 1 (send text position-paragraph start))]
              [init-col (+ 1 (- start
                                (send text paragraph-start-position (send text position-paragraph start))))])
          (basis:process/zodiac
           (parameterize ([read-case-sensitive (basis:setting-case-sensitive? setting)])
             ((basis:zodiac-reader)
              (gui-utils:read-snips/chars-from-text text start end)
              (zodiac:make-location init-line
                                    init-col
                                    start
                                    file)
              #t 1))
           f
           annotate?)))
      
      (define (process-text/no-zodiac text f start end)
        (let* ([buffer-thunk (gui-utils:read-snips/chars-from-text text start end)]
               [snip-string (string->list " 'non-string-snip ")]
               [port-thunk (let ([from-snip null])
                             (rec port-thunk
                               (lambda ()
                                 (if (null? from-snip)
                                     (let ([next (buffer-thunk)])
                                       (if (or (char? next) (eof-object? next))
                                           next
                                           (begin (set! from-snip snip-string)
                                                  (port-thunk))))
                                     (begin0 (car from-snip)
                                             (set! from-snip (cdr from-snip)))))))]
               [port (make-input-port port-thunk (lambda () #t) void)])
          (basis:process/no-zodiac (lambda () ((basis:raw-reader) port)) f)))
      
      (define process-text ; =User=, =Handler=, =No-Breaks=
        (lambda (text fn start end annotate? text-is-file?)
          (if (basis:zodiac-vocabulary? (basis:current-setting))
              (process-text/zodiac text fn start end annotate? text-is-file?)
              (process-text/no-zodiac text fn start end))))
      
      (define (drscheme-load-handler filename)
        (unless (string? filename)
          (raise (raise-type-error
                  'drscheme-load-handler
                  "string"
                  filename)))
        (let* ([p (open-input-file filename)]
               [loc (zodiac:make-location basis:initial-line
                                          basis:initial-column
                                          basis:initial-offset
                                          filename)]
               [chars (begin0
                        (list (read-char p)
                              (read-char p)
                              (read-char p)
                              (read-char p))
                        (close-input-port p))])
          (if (equal? chars (string->list "WXME"))
              (let ([process-sexps
                     (let ([last (list (void))])
                       (lambda (sexp recur)
                         (cond
                           [(basis:process-finish? sexp) last]
                           [else
                            (set! last
                                  (call-with-values
                                   (lambda () (basis:syntax-checking-primitive-eval sexp))
                                   (lambda x x)))
                            (recur)])))])
                (apply values 
                       (let ([text (make-object text%
				     ;;drscheme:text:text%
                                     )])
		     ;;(parameterize ([current-eventspace
		     ;; to get the right snipclasses
		     ;;drscheme:init:system-eventspace])
                         (send text load-file filename)
		       ;;)
                         (begin0
                           (process-text text process-sexps
                                         0 
                                         (send text last-position)
                                         #t
                                         #f)
                      ;; this closes the editor when it
		      ;; is a drscheme:text:text% editor
		      ;; (send text on-close)
                           ))))
              (basis:drscheme-load-handler filename)))))))
