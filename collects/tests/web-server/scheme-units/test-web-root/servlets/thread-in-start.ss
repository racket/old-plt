;;   1. Servlet that starts a thread within dynamic extent of start function                                                                        
;;   2. Wait for first instance to time out.                                      
;;   3. Start a new instance.                                                     
;;   4. Confirm that thread has been shutdown
(module thread-in-start mzscheme
  (require (lib "servlet.ss" "web-server"))

  (provide start timeout interface-version)

  (define timeout 5)

  (define interface-version 'v1)

  (define i 0)

  ;; Open a port and write to it continuously. The testbed reads from this
  ;; port; when the port closes, the thread has been shutdown.

  (define (start req)
    (thread
      (lambda ()
        (set! i (add1 i))
        (let loop ()
          (when (file-exists? "/tmp/thread-in-start")
            (delete-file "/tmp/thread-in-start"))
          (call-with-output-file
            "/tmp/thread-in-start"
            (lambda (op) (display i op)))
          (sleep 1)
          (loop))))
    '(html (head (title "Thread in (start)"))
           (body (h1 "Thread in (start)"))))

  )
