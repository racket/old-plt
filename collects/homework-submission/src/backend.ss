;; Interact with the stored data, the filesystem, etc. This can be swapped out
;; with another backend by changing the definitions of these procedures.

(module backend mzscheme
  (require (lib "contract.ss")
           "md5.ss"
           )

  (provide/contract
    (valid-username-password? (string? string? . -> . boolean?))
    )

  ;; Does the username and password exist in the database? The password passed
  ;; in here is not yet encrypted; this procedure encrypts it, then checks that.
  ;; Invariant: the username is unique.
  (define (valid-username-password? username password)
    (let ((passwd (load "../etc/passwd"))
          (encrypted (md5 password)))
      (let loop ((l passwd))
        (if (null? l)
          #f
          (let ((user-pass (car l)))
            (if (string=? username (car user-pass))
              (string=? encrypted (cadr user-pass))
              (loop (cdr l))))))))

  )
