(module client mzscheme
  (require "board.ss"
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
			       ;;(printf "~a~n" (read-line input))
			       ;;(printf "~a~n" (read-line input))
			       ;;(printf "~a~n" (read-line input))
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
;;      (printf "~a~n" (map
;;		      (lambda (p)
;;			(list (package-id p)
;;			      (package-x p)
;;			      (package-y p)
;;			      (package-weight p)))
;;		      packages))
      (cond
       ((null? packages) (fix-home!)))
      (cond
       (baseline? (send-command (compute-baseline-move packages robots) out))
       (else
        (let ((command (compute-move packages robots)))
          (when (eq? (command-command command) 'p)
            (printf "Robot ~a is picking up ~a~n" (player-id) (map package-id (command-arg command))))
          (when (eq? (command-command command) 'd)
            (printf "Robot ~a is dropping ~a for score ~a~n" 
                    (player-id) 
                    (map package-id (command-arg command))
                    (apply + (map package-weight (command-arg command)))))
;          (when (or (eq? (command-command command) 'n)
;                    (eq? (command-command command) 's)
;                    (eq? (command-command command) 'e)
;                    (eq? (command-command command) 'w))
;            (printf "Robot ~a is moving ~a from ~a,~a~n" (player-id) 
;                    (command-command command) (get-player-x) (get-player-y)))
          (send-command command out))))
      (let ((robots (read-response! update-score
				    packages
				    in)))
	(loop (read-packages in) robots))))
  )