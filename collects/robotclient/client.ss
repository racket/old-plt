(module client mzscheme
  (require "board.ss"
           (lib "class.ss")
           "baseline.ss"
	   "weights.scm"
	   "client-parameters.ss"
           "search.ss")
  (provide start-client)
  
  (define (start-client gui? host-name port)
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
		       input output)))))))

  (define (read-packages in)
    (let* ((x (read-line in))
           (in (open-input-string x)))
      (let loop ((id (read in)))
        (cond
          ((eof-object? id) null)
          ((eq? 'Robot id) (raise 'dead-robot))
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
       
  (define (compute-move-ff packages robots out)
    (let ((command (compute-move packages robots)))
      ;;(printf "Sending command: ~a (bid ~a)~n" (command-command command)
      ;;	  (command-bid command))
      (printf "fight or flight~n")
      (send-command command out)))
  
  (define (do-turn update-score in out)
    (let loop ((packages (read-packages in))
               (robots null))
      (cond
       ((gui)
	(for-each
	 (lambda (p)
	   (send (gui) log (format "Package on ground at (~a, ~a): ~a" 
				   (get-player-x)
				   (get-player-y)
				   (package->string p))))
	 packages)))

      (cond
        ((null? packages) (fix-home!)))

      (cond
       ((is-robot-within? (get-player-x) (get-player-y) 3)
	(cond
	 ((or (null? (path)) (null? (cdr (path))))
	  (compute-baseline-move packages robots)))
	(path-loc (cadr (path)))
        (compute-move-ff packages robots out))
       (else
        (let ((c (compute-baseline-move packages robots)))
          (cond
            ((symbol? c)
             (compute-move-ff packages robots out))
            (else
             (printf "baseline~n")
             (send-command c out))))))

      (let ((robots (read-response! update-score
				    packages
				    in)))
	(loop (read-packages in) robots))))
  )