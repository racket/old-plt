;; Create the necessary data to run tests and give demonstrations.
(module create-data mzscheme
  (require (lib "class.ss")
           (lib "spgsql.ss" "spgsql")
           "../src/backend.ss")

  (provide
    cleanup
    setup
    db-do
    db-currval

    cid
    pid1
    pid2
    pid3
    pid4
    pid5
    pid6
    )


  ;; Globals to make life easier:
  (define cid #f)
  (define pid1 #f)
  (define pid2 #f)
  (define pid3 #f)
  (define pid4 #f)
  (define pid5 #f)
  (define pid6 #f)
  (define aid1 #f)
  (define aid2 #f)
  (define aid3 #f)
  (define aid4 #f)

  (define (cleanup)
    (db-do (string-append
             "DELETE FROM courses WHERE name = 'The Test Course' "
             "AND number = 'TTT000'"))
    (db-do (string-append
             "DELETE FROM people WHERE name LIKE 'Person %' "
             "AND neu_id = 111223333 AND (username LIKE 'staff %' "
             "OR username LIKE 'student %' OR username LIKE 'instructor %' "
             "OR username LIKE 'person %') "
             "AND password = crypt('password',password)"))
    ;; course_people and partners are taken care of beause they depend on
    ;; courses and people.
    )

  ;; Set up the database into some known state:
  ;; - A course
  ;; - Four students, one instructor, and one non-student in this course
  ;; - Two of the students are partners
  ;; - Two past-due assignments for the course
  ;; - Two upcoming assignments for the course
  ;; - Submit a past-due and an upcoming for each student with a partner
  (define (setup)
    ;; A course
    (db-do
      (string-append
        "INSERT INTO courses (name, number, default_partnership_size, directory) "
        "VALUES ('The Test Course', 'TTT000', 2, '/tmp/netgeek/ttt000')"))
    (set! cid (db-currval "courses"))

    ;; Six people. Written out because Scheme ain't as cool as Perl
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person one', 111223333, 'student one', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid1 (db-currval "people")))
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person two', 111223333, 'student two', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid2 (db-currval "people")))
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person three', 111223333, 'student three', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid3 (db-currval "people")))
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person four', 111223333, 'student four', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid4 (db-currval "people")))
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person five', 111223333, 'staff one', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid5 (db-currval "people")))
    (begin
      (db-do
        (string-append
          "INSERT INTO people (name, neu_id, username, password) "
          "VALUES ('Person six', 111223333, 'instructor one', "
          "crypt('password',gen_salt('md5')))"))
      (set! pid6 (db-currval "people")))

    ;; Four students
    (for-each
      (lambda (n)
        (db-do
          (format
            (string-append
              "INSERT INTO course_people (person_id, course_id, position) "
              "VALUES (~a, ~a,'student')")
            n cid)))
      (list pid1 pid2 pid3 pid4))

    ;; One instructor
    (for-each
      (lambda (n)
        (db-do
          (format
            (string-append
              "INSERT INTO course_people (person_id, course_id, position) "
              "VALUES (~a, ~a,'instructor')")
            n cid)))
      (list pid6))

    ;; One non-student
    (for-each
      (lambda (n)
        (db-do
          (format
            (string-append
              "INSERT INTO course_people (person_id, course_id, position) "
              "VALUES (~a, ~a,'instructor')")
            n cid)))
      (list pid5))

    ;; Two of the students are partners
    (let ((pid (send *connection* query-value "SELECT max(partner_id)+1 FROM partners")))
      (for-each
        (lambda (n)
          (db-do 
            (format
              (string-append
                "INSERT INTO partners "
                "(student_id, partner_id, course_id, created, can_submit) "
                "VALUES (~a, ~a,~a,now(),'t')")
              n pid cid)))
        (list pid1 pid2)))

    ;; Two of the students are their own partner
    ;;; This maintains the INVARIANT specified in backend.ss
    (for-each
      (lambda (n)
        (db-do 
          (format
            (string-append
              "INSERT INTO partners "
              "(student_id, partner_id, course_id, created, can_submit) "
              "VALUES (~a, (SELECT max(partner_id)+1 FROM partners),~a,now(),'f')")
            n cid)))
      (list pid3 pid4))

    ;; Two past-due assignment.
    (db-do
      (format
        (string-append
          "INSERT INTO assignments "
          "(name, due, course_id, description, "
          "description_url, grade_type, grade_misc) "
          "VALUES ('The Test Assignment One', '2000-12-01 00:01:11', ~a, "
          "NULL, 'http://mike-burns.com', 'letter', '')")
        cid))
    (set! aid1 (db-currval "assignments"))
    (db-do
      (format
        (string-append
          "INSERT INTO assignments "
          "(name, due, course_id, description, "
          "description_url, grade_type, grade_misc) "
          "VALUES ('The Test Assignment Three', '2000-12-01 00:01:00', ~a, "
          "NULL, 'http://mike-burns.com', 'letter', '')")
        cid))
    (set! aid3 (db-currval "assignments"))

    ;; Two upcoming assignment.
    (db-do
      (format
        (string-append
          "INSERT INTO assignments "
          "(name, due, course_id, description, "
          "description_url, grade_type, grade_misc) "
          "VALUES ('The Test Assignment Two', '2100-12-01 00:01:11', ~a, "
          "NULL, 'http://mike-burns.com', 'letter', '')")
        cid))
    (set! aid2 (db-currval "assignments"))
    (db-do
      (format
        (string-append
          "INSERT INTO assignments "
          "(name, due, course_id, description, "
          "description_url, grade_type, grade_misc) "
          "VALUES ('The Test Assignment Four', '2100-12-01 00:01:00', ~a, "
          "NULL, 'http://mike-burns.com', 'letter', '')")
        cid))
    (set! aid4 (db-currval "assignments"))

    ;; Submit a past-due and upcoming for each student with a partner
    (db-do
      (format
        (string-append
          "INSERT INTO assignment_grades "
          "(assignment_id, partner_id, submission_date, submission, "
          " grade, comment) "
          "VALUES (~a, "
          " (SELECT partner_id FROM partners WHERE student_id = ~a),"
          " now(), '/etc/passwd', 'D', 'It stinks')")
        aid1 pid1))
    (db-do
      (format
        (string-append
          "INSERT INTO assignment_grades "
          "(assignment_id, partner_id, submission_date, submission, "
          " grade, comment) "
          "VALUES (~a, "
          " (SELECT partner_id FROM partners WHERE student_id = ~a),"
          " now(), '/etc/passwd', 'B', 'It''s ... okay')")
        aid2 pid1))

    )


  ;; db-do : String -> void
  ;; Execute the SQL statement.
  (define (db-do sql)
    (send *connection* exec sql))

  ;; db-currval : String -> Number
  ;; The current sequence identifier for the table.
  (define (db-currval table)
    (send *connection* query-value (format "SELECT currval('~a_id_seq')" table)))

  )
