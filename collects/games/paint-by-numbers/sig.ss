#|

TODO:

 - make each player frame specific to a board (more vertical space)
 - add collections of puzzles
 - add a feature that takes a list of saved editor files and makes a puzzle collection
 - add resize and "shrink to fit" features to editor

Annoying bugs to be fixed:

 - In large boards, the off-by-1 spacing for the groups of 5 probably makes a bug.
   Make groups have size 1 to test
 - In editors, when a column label has an entry with a 10 or more, the sizing is not
   right. Need to check this case and resize the columns.

|#


#|

types:

  (define-type cols (listof (listof number)))
  (define-type rows (listof (listof number)))

  (define-type solution (vector-of (vector-of (union 'on 'off 'unknown))))

  (define-type problem (make-problem string cols rows (union #f solution)))

|#

(require-library "mred-interfaces.ss" "framework")
(require-library "frameworks.ss" "framework")
(require-library "raw-sig.ss" "games" "paint-by-numbers")
(require-library "errortrace.ss" "errortrace")

