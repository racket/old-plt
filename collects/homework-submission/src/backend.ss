;; Interact with the stored data, the filesystem, etc. This can be swapped out
;; with another backend by changing the definitions of these procedures.

(module backend mzscheme
  (require (lib "contract.ss")
           "md5.ss"
           )

  (provide/contract
    (valid-username-password? (string? string? . -> . boolean?))
    (update-password! (string? string? . -> . any))
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

  ;; Update the password database with the new password, encrypted.
  (define (update-password! username password)
    (let ((new-passwds (update-passwords username (md5 password))))
      (when (file-exists? "../etc/passwd")
        (delete-file "../etc/passwd"))
      (call-with-output-file
        "../etc/passwd"
        (lambda (op)
          (fprintf op "'~s~n" new-passwds)))))

  ;; Produce a new list of username/password pairs with the new password
  ;; in place of the old password.
  (define (update-passwords username password)
    (let loop ((passwords (load "../etc/passwd")))
      (if (null? passwords)
        '()
        (let ((user-pass (car passwords)))
          (if (string=? username (car user-pass))
            (cons (list username password) (cdr passwords))
            (cons user-pass (loop (cdr passwords))))))))

  )
