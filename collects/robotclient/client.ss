(module client mzscheme
  (require "board.ss"
           "baseline.ss"
           "search.ss")
  (provide start-client)
  
  (define (start-client baseline? host-name port)
    ;;(with-handlers ((exn? (lambda (ex) (printf "exception: ~a~n" ex))))
      (let-values (((input output) (tcp-connect host-name port)))
        (display "Player" output)
        (newline output)
        (read-board! input)
        (do-turn baseline? input output)))

  (define (read-packages in)
    (let ((in (open-input-string (read-line in))))
      (let loop ((id (read in)))
        (cond
          ((eof-object? in) null)
          (else
           (cons
            (make-package id (read in) (read in) (read in))
            (loop (read in))))))))
  
  (define (send-command command out)
    (display (command-bid command) out)
    (case (command-command command)
      ((n) (display "Move N" out))
      ((s) (display "Move S" out))
      ((e) (display "Move E" out))
      ((w) (display "Move W" out))
      ((p) 
       (display "Pick " out)
       (for-each (lambda (x)
                   (display x out)
                   (display " " out))
                 (command-arg command)))
      ((d)
       (display "Drop " out)
       (for-each (lambda (x)
                   (display x out)
                   (display " " out))
                 (command-arg command))))
    (newline out))
       
  
  
  (define (do-turn baseline? in out)
    (let loop ((packages (read-packages in))
               (robots null))
      (cond
        ((null? packages) (fix-home!)))
      (cond
        (baseline? (send-command (compute-baseline-move packages robots) out))
        (else
         (send-command (compute-move packages robots) out)))
      (let ((robots (read-response! packages in)))
        (loop (read-packages in) robots))))
  )