;;   1. Servlet that starts a thread at load time.                                
;;   2. Refresh the servlet scripts.
;;   4. Confirm that the thread was shutdown.
(module thread-at-load mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 5)

  (define interface-version 'v1)

  (define i 0)

  (define the-thread
    (thread
      (lambda ()
        (set! i (add1 i))
        (let loop ()
          (when (file-exists? "/tmp/thread-at-load")
            (delete-file "/tmp/thread-at-load"))
          (call-with-output-file
            "/tmp/thread-at-load"
            (lambda (op) (display i op)))
          (sleep 1)
          (loop)))))

  (define (start req)
    `(html (head (title "Thread at Load"))
           (body (h1 "Thread at Load")
                 (p ,(if (thread-dead? the-thread) "true" "false")))))

  )
