(module client mzscheme
  (require "board.ss"
           (lib "class.ss")
           "baseline.ss"
	   "client-parameters.ss"
           "search.ss")
  (provide start-client)
  
  (define (start-client baseline? gui? host-name port)
    (let ((client-custodian (make-custodian)))
      (parameterize ((current-custodian client-custodian))
        (with-handlers ((exn? (lambda (ex)
                                (custodian-shutdown-all client-custodian)
				;;(printf "~a~n" (exn-message ex))
				;;(display (score))(newline)
                                (raise ex)
                                (score))))
          (let-values (((input output) (tcp-connect host-name port)))
	    (with-handlers (((lambda (ex)
			       (eq? 'dead-robot ex))
			     (lambda (ex)
                               (cond
                                 ((gui)
                                  (send (gui) log (read-line input))
                                  (send (gui) log (read-line input))
                                  (send (gui) log (read-line input))))
			       (close-output-port output)
			       (close-input-port input)
			       (score))))
	      (init-parameters)
	      (display "Player" output)
	      (newline output)
	      (read-board! input gui?)
	      (do-turn (lambda (x) (score (+ (score) x))) 
		       baseline? input output)))))))

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
    (player-money (- (player-money) (abs (command-bid command))))
    (case (command-command command)
      ((n) (display " Move N" out))
      ((s) (display " Move S" out))
      ((e) (display " Move E" out))
      ((w) (display " Move W" out))
      ((p) 
       (display " Pick " out)
       (for-each (lambda (x)
                   (display (package-id x) out)
                   (display " " out))
                 (command-arg command)))
      ((d)
       (display " Drop " out)
       (for-each (lambda (x)
                   (display (package-id x) out)
                   (display " " out))
                 (command-arg command))))
    (newline out))
       
  
  
  (define (do-turn update-score baseline? in out)
    (let loop ((packages (read-packages in))
               (robots null))
      (cond
       ((null? packages) (fix-home!)))
      (cond
       (baseline? (send-command (compute-baseline-move packages robots) out))
       (else
        (let ((command (compute-move packages robots)))
          (send-command command out))))
      (let ((robots (read-response! update-score
				    packages
				    in)))
	(loop (read-packages in) robots))))
  )