
(define child? (member "child" (vector->list (current-command-line-arguments))))

(unless child?
  (load-relative "loadtest.ss")

  (let ([path (find-executable-path (find-system-path 'exec-file) #f)])
    (let-values ([(subproc in out /err) (subprocess #f #f (current-error-port)
                                                    path "-rq"  
                                                    (build-path (current-load-relative-directory)
                                                                "subproc2.ss")
                                                    "child")])
      (test 'running subprocess-status subproc)
      (test out object-wait-multiple 0 out)
      (test #f object-wait-multiple 0 in)
      (fprintf out "go~n")
      
      (test "going" read-line in)
      
      (test #t
            positive?
            ;; Push 50000 chars; should block at least once:
            (let ([s (make-string 50000 #\a)])
              (let loop ([offset 0])
                (let ([ready? (object-wait-multiple 0 out)])
                  (printf "~a ~a~n" offset ready?)
                  (+ (if ready? 0 1)
                     (let ([n (write-string-avail s out offset)])
                       (if (= (+ n offset) 50000)
                           0
                           (loop (+ offset n)))))))))
      
      'ok)))

(when child?
  (with-handlers ([void (lambda (x)
                          (fprintf (current-error-port) "CHILD ")
                          (raise x))])
    (if (equal? "go" (read-line (current-input-port) 'any))
        (printf "going~n")
        (printf "not go!~n"))
  
    (fprintf (current-error-port) "CHILD: starting sleep~n")
    (sleep 3)
    (fprintf (current-error-port) "CHILD: finished sleep; reading...~n")

    (unless (= 50000 (string-length (read-string 50000)))
      (fprintf (current-error-port) "CHILD: bad read count"))
    
    'ok))
