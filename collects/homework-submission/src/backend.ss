;; Interact with the stored data, the filesystem, etc. This can be swapped out
;; with another backend by changing the definitions of these procedures.

;;; INVARIANT: A student in a course must be in the partners table, even if he
;;;            or she is alone and cannot submit.
;;;            This should be maintained by the database. Figure this out.

(module backend mzscheme
  (require (lib "contract.ss")
           (lib "class.ss")
           (lib "file.ss")
           (lib "spgsql.ss" "spgsql")
           (lib "13.ss" "srfi")
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
    (can-add-partner? (number? number? . -> . boolean?))
    (partners (number? number? . -> . (listof string?)))
    (user-in-course? (string? number? . -> . boolean?))
    (add-partner! (number? number? string? . -> . any))
    (can-submit? (number? number? . -> . boolean?))
    (id/username (string? . -> . number?))
    (partnership-full? (number? number? . -> . boolean?))
    (assignments/due (number? number? symbol? . -> . (listof assignment?)))
    (submission-filename (number? number? number? . -> . path?))
    (store-submission/file! (path? string? . -> . any))
    (store-submission/db! (number? number? path? . -> . any))
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
    (select-structure
      (format
        (string-append
          "SELECT c.id, c.name, c.number, "
          "c_p.position, pt.can_submit, "
          "count(pt.partner_id) >= c.default_partnership_size "
          "FROM courses c "
          "JOIN course_people c_p ON c.id = c_p.course_id "
          "JOIN people p ON p.id = c_p.person_id "
          "LEFT JOIN partners pt ON pt.student_id = p.id "
          "WHERE pt.partner_id = "
          "(SELECT pt2.partner_id FROM partners pt2 "
          "JOIN people p2 ON p2.id = pt2.student_id "
          "WHERE p2.username = '~a' AND pt2.ended IS NULL)"
          "OR pt.partner_id IS NULL "
          "AND pt.ended IS NULL "
          "AND p.username = '~a' "
          "GROUP BY c.id, c.name, c.number, c_p.position, pt.can_submit, "
          "c.default_partnership_size")
        username username)
      row->course))

  ;; The ID for a given username.
  (define (id/username username)
    (send *connection* query-value
          (format "SELECT id FROM people WHERE username = '~a'"
                  username)))

  ;; Can the student add a partner?
  (define (can-add-partner? sid cid)
    (exists? "partners pt"
             (format
               (string-append
                 "pt.ended IS NULL "
                 "AND pt.partner_id = "
                 " (SELECT partner_id FROM partners "
                 "  WHERE student_id = ~a AND ended IS NULL) "
                 "AND pt.course_id = ~a "
                 "HAVING count(pt.partner_id) < "
                 " (SELECT default_partnership_size "
                 "  FROM courses WHERE id = ~a)")
               sid cid cid)))

  ;; All the partners for a student in a course
  (define (partners sid cid)
    (map bytes->string/utf-8
         (send *connection* query-list
               (format 
                 (string-append
                   "SELECT p.name "
                   "FROM people p "
                   "JOIN partners pt ON pt.student_id = p.id "
                   "WHERE pt.ended IS NULL "
                   "AND pt.partner_id =  "
                   " (SELECT partner_id FROM partners "
                   "  WHERE student_id = ~a AND ended IS NULL) "
                   "AND pt.course_id = ~a "
                   "AND p.id != ~a")
                 sid cid sid))))

  ;; Is the user in the course?
  (define (user-in-course? username cid)
    (exists?
      (string-append
        "people p "
        "JOIN course_people c_p ON c_p.person_id = p.id "
        "JOIN courses c ON c.id = c_p.course_id")
      (format "p.username = '~a' AND c.id = ~a" username cid)))

  ;; Add the student as a partner for this student.
  (define (add-partner! sid cid username)
    (let* ((p-sid (send *connection* query-value
                        (format "SELECT id FROM people WHERE username = '~a'"
                                username)))
           (can-submit? 
             (exists? "courses c JOIN partners pt ON pt.course_id = c.id"
                      (format 
                        (string-append
                          "pt.course_id = ~a "
                          "AND (pt.student_id = ~a OR pt.student_id = ~a) "
                          "AND pt.ended IS NULL "
                          "GROUP BY c.default_partnership_size "
                          "HAVING c.default_partnership_size = (max(pt.partner_id) + 1) ")
                        cid sid p-sid))))
      (send *connection* exec
            (format
              (string-append
                "UPDATE partners SET ended = now() "
                "WHERE course_id = ~a "
                "AND ended IS NULL "
                "AND (student_id = ~a OR student_id = ~a)")
              cid sid p-sid))
      (send *connection* exec
            (format
              (string-append
                "INSERT INTO partners "
                "(student_id, partner_id, course_id, created, can_submit) "
                "VALUES (~a, (SELECT max(partner_id) + 1 FROM partners), "
                "~a, now(), '~a')")
              sid cid (if can-submit? 't 'f)))
      (send *connection* exec
            (format
              (string-append
                "INSERT INTO partners "
                "(student_id, partner_id, course_id, created, can_submit) "
                "VALUES (~a, (SELECT max(partner_id) FROM partners), "
                "~a, now(), '~a')")
              p-sid cid (if can-submit? 't 'f)))))

  ;; A student can submit assignments if the can_submit field is true.
  (define (can-submit? sid cid)
    (exists? "partners pt"
             (format
               (string-append
                 "pt.can_submit = 't' "
                 "AND pt.ended IS NULL "
                 "AND pt.course_id = ~a "
                 "AND pt.student_id = ~a")
               cid sid)))

  ;; Can the student modify his or her partnership?
  (define (partnership-full? sid cid)
    (exists? "partners pt JOIN courses c ON c.id = pt.course_id"
             (format
               (string-append
                 "pt.partner_id = "
                 " (SELECT partner_id FROM partners "
                 "  WHERE student_id = ~a AND ended IS NULL) "
                 "AND pt.course_id = ~a "
                 "GROUP BY c.default_partnership_size "
                 "HAVING count(pt.partner_id) >= c.default_partnership_size")
               sid cid)))

  ;; Assignments for a course where the due date has or has not yet exipred.
  ;; NOTE: This took me way too long to write. OTOH, once I figured it out
  ;;       it was easy.
  (define (assignments/due sid cid cmp)
    (select-structure
      (format
        (string-append
          "SELECT DISTINCT "
          "a.id, a.name, a.due, a.description, a.description_url, "
          "a.grade_type, a.grade_misc, pt.partner_id, a_g.submission_date, "
          "a_g.submission, a_g.grade, a_g.comment "
          "FROM courses c "
          "JOIN partners pt ON pt.course_id = c.id "
          "JOIN people p ON p.id = pt.student_id "
          "JOIN assignments a ON a.course_id = c.id "
          "LEFT JOIN assignment_grades a_g ON a_g.assignment_id = a.id "
          "WHERE p.id = ~a "
          "AND c.id = ~a"
          "AND a.due ~a now() "
          "ORDER BY a.due "
          (if (equal? cmp '<) "ASC" "DESC"))
        sid cid cmp)
      row->assignment))

  ;; Generate the filename used to save the file on the disc.
  ;; ${course-directory}/hw/${assignment-name}/${student-names}-random
  (define (submission-filename sid cid aid)
    (let  ((course-directory 
             (send *connection* query-value
                   (format "SELECT directory FROM courses WHERE id = ~a" cid)))
           (assignment-name
             (send *connection* query-value
                   (format "SELECT name FROM assignments WHERE id = ~a" aid)))
           (student-names
             (send *connection* query-list
                   (format
                     (string-append
                       "SELECT p.name "
                       "FROM people p "
                       "JOIN partners pt ON pt.student_id = p.id "
                       "WHERE pt.ended IS NULL "
                       "AND pt.partner_id = "
                       "(SELECT partner_id FROM partners WHERE student_id = ~a)")
                     sid))))
      (build-path
        (bytes->string/utf-8 course-directory)
        "hw"
        (bytes->string/utf-8 assignment-name)
        (string-append
          (string-join (map bytes->string/utf-8 student-names) ", ")
          "-"
          (number->string (random 1000))))))

  ;; Store the file on the filesystem.
  (define (store-submission/file! filename contents)
    (let-values (((base name must-be-dir?) (split-path filename)))
      (make-directory* base))
    (when (file-exists? filename) (delete-file filename))
    (with-output-to-file
      filename
      (lambda () (display contents))))

  ;; Store information about the file in the database
  (define (store-submission/db! pid aid filename)
    (if (exists? "assignment_grades"
                 (format "assignment_id = ~a AND partner_id = ~a" aid pid))
      (send *connection* exec
            (format
              (string-append
                "UPDATE assignment_grades SET submission = '~a' "
                "WHERE assignment_id = ~a AND partner_id = ~a")
              (dbify (path->string filename)) aid pid))
      (send *connection* exec
            (format
              (string-append
                "INSERT INTO assignment_grades "
                "(assignment_id, partner_id, submission) "
                "VALUES (~a,~a,'~a')")
              aid pid (dbify (path->string filename))))))

  ;; ******************************************************************

  ;; Assignments for a student in a course where the due date has not yet
  ;; exipred.
  (define (select-structure sql vector->struct)
    (let ((q (send *connection* query sql)))
      (cond
        ( (RecordSet? q)
          (map vector->struct (RecordSet-rows q)) )
        ( (ErrorResult? q)
          (raise (make-exn:fail (string->immutable-string
                                  (format "An error occured: ~s~n"
                                          (ErrorResult-message q)))
                                (current-continuation-marks))) )
        ( else (raise (make-exn:fail (string->immutable-string
                                       (format "Unknown error: ~s~n" q))
                                     (current-continuation-marks))) ))))

  ;; Convert a row represented as a vector into a course.
  (define (row->course r)
    (make-course (vector-ref r 0)
                 (bytes->string/utf-8 (vector-ref r 1))
                 (bytes->string/utf-8 (vector-ref r 2))
                 (string->symbol (bytes->string/utf-8 (vector-ref r 3)))
                 (let ((v (vector-ref r 4))) (if (sql-null? v) #f v))
                 (vector-ref r 5)))

  ;; Convert a row represented as a vector into an assignment.
  (define (row->assignment r)
    (make-assignment (vector-ref r 0)
                     (bytes->string/utf-8 (vector-ref r 1))
                     (bytes->string/utf-8 (vector-ref r 2))
                     (value/null (vector-ref r 3) "")
                     (value/null (vector-ref r 4) "")
                     (string->symbol (bytes->string/utf-8 (vector-ref r 5)))
                     (bytes->string/utf-8 (vector-ref r 6))
                     (vector-ref r 7)
                     (value/null (vector-ref r 8) #f)
                     (value/null (vector-ref r 9) #f)
                     (value/null (vector-ref r 10) #f)
                     (value/null (vector-ref r 11) #f)))

  ;; The value if it is not sql-null, or some default value.
  (define (value/null v d)
    (cond
      ( (sql-null? v) d )
      ( (bytes? v) (bytes->string/utf-8 v) )
      ( else v )))

  ;; exists? : String String -> Boolean
  ;; Is something known and existant in the database?
  (define (exists? table where)
    (let ((q (send *connection* query
                   (string-append "SELECT 1 FROM " table " WHERE " where))))
      (cond
        ( (RecordSet? q) (not (= (length (RecordSet-rows q)) 0)) )
        ( (ErrorResult? q) (raise (make-exn:fail
                                    (string->immutable-string
                                      (bytes->string/utf-8
                                        (ErrorResult-message q)))
                                    (current-continuation-marks))) )
        ( else (raise (make-exn:fail (string->immutable-string
                                       (format "Got: ~s~n" q))
                                    (current-continuation-marks))) ))))

  ;; dbify : String -> String
  ;; Escape any non-SQL-safe characters
  (define (dbify q)
    (regexp-replace* (regexp "([^'])'([^'])") q "\\1''\\2"))

  )
