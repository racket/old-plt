#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "class.ss")
         "../src/backend.ss")

;; db-do : String -> void
;; Execute the SQL statement.
(define (db-do sql)
  (send *connection* exec sql))

;; Cleanup
(db-do "DELETE FROM people WHERE name = 'Unmade User'")
(db-do "DELETE FROM people WHERE name = 'The Test User'")
(db-do "DELETE FROM courses WHERE name = 'The Test Course'")
(db-do (string-append
         "DELETE FROM course_people WHERE "
         "person_id IN (SELECT id FROM people WHERE name = 'Unmade User' OR name = 'The Test User') AND "
         "course_id = (SELECT id FROM courses WHERE name = 'The Test Course')"))

;; Add
(add-user! "Unmade User" 1234)
(add-user! "The Test User" 1111)
(create-account! "The Test User" 1111 "The Test Username" "The Test Password")

(db-do "INSERT INTO courses (name, number) VALUES ('The Test Course','CSU000')")

(db-do (string-append
         "INSERT INTO course_people (person_id, course_id) "
         "VALUES ((SELECT id FROM people WHERE name = 'Unmade User'), "
         "(SELECT id FROM courses WHERE name = 'The Test Course'))"))

(db-do (string-append
         "INSERT INTO course_people (person_id, course_id) "
         "VALUES ((SELECT id FROM people WHERE name = 'The Test User'), "
         "(SELECT id FROM courses WHERE name = 'The Test Course'))"))
