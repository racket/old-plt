(module client mzscheme
  (require "board.ss"
           "baseline.ss"
           "search.ss")
  (provide start-client)
  
  (define (start-client baseline? gui? host-name port)
    ;;(with-handlers ((exn? (lambda (ex) (printf "exception: ~a~n" ex))))
      (let-values (((input output) (tcp-connect host-name port)))
        (display "Player" output)
        (newline output)
        (read-board! input (cond
                             (gui? (dynamic-require "gui-client.ss" 'initialize))
                             (else void)))
        (do-turn baseline? gui? input output)))

  (define (read-packages in)
    (let* ((x (read-line in))
           (in (open-input-string x)))
      (let loop ((id (read in)))
        (cond
          ((eof-object? id) null)
          (else
           (cons
            (make-package id (read in) (read in) (read in))
            (loop (read in))))))))
  
  (define (send-command command out)
    (display (command-bid command) out)
    (case (command-command command)
      ((n) (display " Move N" out))
      ((s) (display " Move S" out))
      ((e) (display " Move E" out))
      ((w) (display " Move W" out))
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
       
  
  
  (define (do-turn baseline? gui? in out)
    (let loop ((packages (read-packages in))
               (robots null))
      (cond
        ((null? packages) (fix-home!)))
      (cond
        (baseline? (send-command (compute-baseline-move packages robots) out))
        (else
         (send-command (compute-move packages robots) out)))
      (let ((robots (read-response! packages in (cond
                                                  (gui? (dynamic-require "gui-client.ss" 'update))
                                                  (else void)))))
        (loop (read-packages in) robots))))
  )