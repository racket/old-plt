;; Does the Web server exit when a servlet calls exit?
(module exit mzscheme
  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)
  (define (start req) (exit) (send/finish "success")))
