;; Data structures common to transitions and pages.
(module data mzscheme
  (require (lib "contract.ss"))

  ;; A Session is a
  ;; (make-session String Course)
  (define-struct session (username course))

  ;; A Course is a
  ;; (make-course String String)
  (define-struct course (name number))

  (provide/contract
    (struct course ((name string?)
                    (number string?)))
    (struct session ((username string?)
                     (course (union course? not))))
    ))
