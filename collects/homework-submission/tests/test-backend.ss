#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

;; Automated tests of the backend of the homework submission servlet.

(require (lib "text-ui.ss" "schemeunit")
         (lib "etc.ss")
         (lib "class.ss")
         (lib "test.ss" "schemeunit")
         (lib "spgsql.ss" "spgsql")

         "../src/data.ss"
         "../src/backend.ss"
         "create-data.ss"
         )

(define-simple-assertion (assert-db t sql)
  (let ((q (send *connection* query sql)))
    (cond
      ( (RecordSet? q) (not (= (length (RecordSet-rows q)) 0)) )
      ( (ErrorResult? q) (raise (make-exn:fail
                                  (string->immutable-string
                                    (bytes->string/utf-8
                                      (ErrorResult-message q)))
                                  (current-continuation-marks))) )
      ( else (raise (make-exn:fail (format "Got: ~s~n" q)
                                   (current-continuation-marks))) ))))

(define backend-test
  (make-test-suite
    "Does the backend perform as expected?"

    (make-test-case
      "courses for a student"
      (assert-equal?
        (courses "person one")
        (list (make-course cid "The Test Course" "TTT000" 'student #t #t))))

    (make-test-case
      "courses for a non-student"
      (assert-equal?
        (courses "person five")
        (list (make-course cid "The Test Course" "TTT000" 'instructor #f #f))))

    (make-test-case
      "partnership-full? for a full partnership"
      (assert-equal? (partnership-full? pid1 cid) #t))

    (make-test-case
      "partnership-full? for a non-full partnership"
      (assert-equal? (partnership-full? pid3 cid) #f))

    (make-test-case
      "can-submit? for student in a partnership of the correct size"
      (assert-equal? (can-submit? pid1 cid) #t))

    (make-test-case
      "can-submit? for student in a partnership of the incorrect size"
      (assert-equal? (can-submit? pid3 cid) #f))

    (make-test-case
      "user-in-course? for a person that is in the course"
      (assert-equal? (user-in-course? "person five" cid) #t))

    (make-test-case
      "user-in-course? for a person that is not in the course"
      (assert-equal? (user-in-course? "person six" cid) #f))

    (make-test-case
      "can-add-partner? for a student who can add a partner"
      (assert-equal? (can-add-partner? pid3 cid) #t))

    (make-test-case
      "can-add-partner? for a student who cannot add a partner"
      (assert-equal? (can-add-partner? pid1 cid) #f))

    (make-test-case
      "partners for a student with a partner"
      (assert-equal? (partners pid1 cid) (list "Person two")))

    (make-test-case
      "partners for a student with no partners"
      (assert-equal? (partners pid3 cid) null))

    (make-test-case
      "add-partner! for a student"
      (assert-db (add-partner! pid3 cid "person four")
                 (format
                   (string-append
                     "SELECT 't' FROM partners pt "
                     "WHERE pt.ended IS NULL "
                     "AND (pt.student_id = ~a OR pt.student_id = ~a) "
                     "HAVING count(pt.partner_id) = 2")
                   pid3 pid4))
      ;; Nothing to set up
      (void)
      ;; Undo their partnership afterwards
      (begin
        (db-do
          (format
            (string-append
              "UPDATE partners SET ended = now() "
              "WHERE student_id = ~a OR student_id = ~a AND ended IS NULL")
            pid3 pid4))
        (for-each
          (lambda (n)
            (db-do 
              (format
                (string-append
                  "INSERT INTO partners "
                  "(student_id, partner_id, course_id, created, can_submit) "
                  "VALUES (~a, (SELECT max(partner_id)+1 FROM partners),~a,now(),'f')")
                n cid)))
          (list pid3 pid4))))

    ))

;; ********************************************************************
(with-handlers ((exn? (lambda (e) (db-do "ROLLBACK") (raise e))))
  (db-do "BEGIN")
  (cleanup)
  (setup)
  (test/text-ui backend-test)
  (cleanup)
  (db-do "COMMIT")
  )
