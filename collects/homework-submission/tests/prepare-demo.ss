#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "class.ss")
         "../src/backend.ss"
         "create-data.ss")
(with-handlers ((exn? (lambda (e) (db-do "ROLLBACK") (raise e))))
  (db-do "BEGIN")
  (cleanup)
  (setup)
  (db-do "COMMIT")
  )

#|
;; db-do : String -> void
;; Execute the SQL statement.
(define (db-do sql)
  (send *connection* exec sql))

;; Cleanup
(db-do "DELETE FROM people WHERE name = 'Unmade User'")
(db-do "DELETE FROM people WHERE name = 'The Test User'")
(db-do "DELETE FROM people WHERE name = 'The Test Student'")
(db-do "DELETE FROM people WHERE name = 'The Test NonStudent'")
(db-do "DELETE FROM courses WHERE name = 'The Test Course'")
(db-do (string-append
         "DELETE FROM course_people WHERE "
         "person_id IN "
         "(SELECT id FROM people "
         "WHERE name = 'Unmade User' "
         "OR name = 'The Test User' "
         "OR name = 'The Test Student' "
         "OR name = 'The Test NonStudent') "
         "AND course_id = "
         "(SELECT id FROM courses WHERE name = 'The Test Course')"))

;; Add
(add-user! "Unmade User" 1234)
(add-user! "The Test Student" 1111)
(create-account! "The Test Student" 1111 "The Test Student" "The Test Password")
(add-user! "The Test NonStudent" 1111)
(create-account! "The Test NonStudent" 1111 "The Test NonStudent" "The Test Password")

(db-do "INSERT INTO courses (name, number) VALUES ('The Test Course','CSU000')")

(db-do (string-append
         "INSERT INTO course_people (person_id, course_id, position) "
         "VALUES ((SELECT id FROM people WHERE name = 'Unmade User'), "
         "(SELECT id FROM courses WHERE name = 'The Test Course'), "
         "'student')"))

(db-do (string-append
         "INSERT INTO course_people (person_id, course_id, position) "
         "VALUES ((SELECT id FROM people WHERE name = 'The Test Student'), "
         "(SELECT id FROM courses WHERE name = 'The Test Course'), "
         "'student')"))

(db-do (string-append
         "INSERT INTO course_people (person_id, course_id, position) "
         "VALUES ((SELECT id FROM people WHERE name = 'The Test NonStudent'), "
         "(SELECT id FROM courses WHERE name = 'The Test Course'), "
         "'instructor')"))
|#
