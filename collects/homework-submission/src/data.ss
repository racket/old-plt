;; Data structures common to transitions and pages.
(module data mzscheme
  (require (lib "contract.ss"))

  ;; A Session is a
  ;; (make-session String Course)
  (define-struct session (username course))

  ;; A Course is a
  ;; (make-course String String Symbol)
  (define-struct course (name number position))

  (provide/contract
    (struct course ((name string?)
                    (number string?)
                    (position symbol?)))
    (struct session ((username string?)
                     (course (union course? not))))
    ))
