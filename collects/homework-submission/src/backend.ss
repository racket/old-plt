;; Interact with the stored data, the filesystem, etc. This can be swapped out
;; with another backend by changing the definitions of these procedures.

(module backend mzscheme
  (require (lib "contract.ss")
           (lib "class.ss")
           (lib "spgsql.ss" "spgsql")
           "data.ss"
           )

  (provide/contract
    (valid-username-password? (string? string? . -> . boolean?))
    (update-password! (string? string? . -> . any))
    (name/neu-id-match? (string? number? . -> . boolean?))
    (username-taken? (string? . -> . boolean?))
    (create-account! (string? number? string? string? . -> . any))
    (db-rollback (-> any))
    (db-commit (-> any))
    (db-begin (-> any))
    (destroy-user! (string? . -> . any))
    (add-user! (string? number? . -> . any))
    (has-username? (string? number? . -> . boolean?))
    (courses (string? . -> . (listof course?)))
    )

  (provide *connection*)

  (define *connection*
    (let ((db-connection (connect "subra.ccs.neu.edu" 8432 #"homework" #"csu211")))
      ;; Convert the data to the correct types
      (send db-connection use-type-conversions #t)
      db-connection))

  ;; Transactions.
  (define (db-rollback) (send *connection* exec "ROLLBACK"))
  (define (db-commit)   (send *connection* exec "COMMIT"))
  (define (db-begin)    (send *connection* exec "BEGIN"))

  ;; Does the username and password exist in the database? The password passed
  ;; in here is not yet encrypted; this procedure encrypts it, then checks that.
  (define (valid-username-password? username password)
    (exists? "people"
             (format "username = '~a' AND password = crypt('~a',password)"
                     (dbify username) (dbify password))))

  ;; Update the password database with the new password, encrypted.
  (define (update-password! username password)
    (send *connection* exec
          (format
            (string-append "UPDATE people "
                           "SET password = crypt('~a',gen_salt('md5')) "
                           "WHERE username = '~a'")
            (dbify password) (dbify username))))

  ;; Is the name and NEU ID a pair in the database?
  (define (name/neu-id-match? name neu-id)
    (exists? "people"
             (format "name = '~a' AND neu_id = ~a" (dbify name) neu-id)))

  ;; Is the username taken?
  (define (username-taken? username)
    (exists? "people" (format "username = '~a'" (dbify username))))

  ;; Create an account with the given name, NEU ID, username, and password. It
  ;; is assumed that the account can be created.
  (define (create-account! name neu-id username password)
    (send *connection* exec
          (format
            (string-append
              "UPDATE people SET username = '~a', "
              "password = crypt('~a',gen_salt('md5')) "
              "WHERE name = '~a' AND neu_id = ~a")
            (dbify username) (dbify password) (dbify name) neu-id)))

  ;; Totally annihilate a user from the database. NOTE: Does this belong here?
  (define (destroy-user! username)
    (send *connection* exec
          (format "DELETE FROM people WHERE username = '~a'"
                  (dbify username))))

  ;; Add a person's name and ID to the database.
  (define (add-user! name neu-id)
    (send *connection* exec
          (format "INSERT INTO people (name, neu_id) VALUES ('~a',~a)"
                  (dbify name) neu-id)))

  ;; Does this user already have a username?
  (define (has-username? name neu-id)
    (exists? "people"
             (format "name = '~a' AND neu_id = ~a AND username IS NOT NULL"
                     (dbify name) neu-id)))

  ;; The courses in which this user is enrolled.
  (define (courses username)
    (let ((q (send *connection* query
                   (format (string-append "SELECT c.name, c.number "
                                          "FROM courses c "
                                          "JOIN course_people c_p "
                                          "ON c.id = c_p.course_id "
                                          "JOIN people p "
                                          "ON p.id = c_p.person_id "
                                          "WHERE p.username = '~a'")
                           username))))
      (cond
        ( (RecordSet? q)
          (map row->course (RecordSet-rows q)) )
        ( (ErrorResult? q)
          (raise (make-exn:fail (format "An error occured: ~s~n"
                                        (ErrorResult-message q))
                              (current-continuation-marks))) )
        ( else (raise (make-exn:fail (format "Unknown error: ~s~n" q)
                                     (current-continuation-marks))) ))))

  ;; ******************************************************************

  ;; Convert a row represented as a vector into a course.
  (define (row->course r)
    (make-course (bytes->string/utf-8 (vector-ref r 0))
                 (bytes->string/utf-8 (vector-ref r 1))))

  ;; exists? : String String -> Boolean
  ;; Is something known and existant in the database?
  (define (exists? table where)
    (not (= (length
              (RecordSet-rows
                (send *connection* query
                      (string-append "SELECT 1 FROM " table " WHERE " where))))
            0)))

  ;; dbify : String -> String
  ;; Escape any non-SQL-safe characters
  (define (dbify q)
    (regexp-replace* (regexp "([^'])'([^'])") q "\\1''\\2"))

  )
