;; Data structures common to transitions and pages.
(module data mzscheme
  (require (lib "contract.ss"))

  ;; A Session is a
  ;; (make-session Number String Course)
  (define-struct session
    (id username course)
    (make-inspector))

  ;; A Course is a
  ;; (make-course Number String String Symbol Boolean Boolean)
  (define-struct course
    (id name number position can-submit? partnership-full?)
    (make-inspector))

  (provide/contract
    (struct course ((id number?)
                    (name string?)
                    (number string?)
                    (position symbol?)
                    (can-submit? boolean?)
                    (partnership-full? boolean?)))
    (struct session ((id number?)
                     (username string?)
                     (course (union course? not))
                     ))
    ))
