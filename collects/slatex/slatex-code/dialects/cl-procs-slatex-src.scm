(scmxlate-ignore 
 *operating-system*
; adjoin
; delete
 delete-if
 directory-namestring
; exit-scheme
 mapcan
 the-setter-for-of
; position-char
 setf)

(defun scheme-like-delete (x l eq)
  (declare (list l))
  (delete x l :test eq))

(defun scheme-like-adjoin (x l eq)
  (declare (list l))
  (adjoin x l :test eq))

(scmxlate-rename-define
 (adjoin #'scheme-like-adjoin)
 (delete #'scheme-like-delete)
 (exit-slatex #'exit)
 (position-char #'position)
 )

(defmacro defenum (&rest z)
  (let ((z z) (i 0) (r '()))
    (loop
     (when (null z) (return `(progn ,@r)))
     (push `(defvar ,(car z) (code-char ,i)) r)
     (incf i)
     (pop z))))

(defmacro defrecord (name &rest fields)
  (let ((fields fields) (i 0) (r '()))
    (loop
     (when (null fields)
       (return
       `(progn
         (defun ,name () (make-array ,i))
         ,@r)))
     (push `(defvar ,(car fields) ,i) r)
     (pop fields)
     (incf i))))

(defmacro of (r i &rest z)
    (cond ((null z) `(elt ,r ,i))
          ((and (eq i '/) (= (length z) 1))
           `(char ,r ,(car z)))
          (t `(of (elt ,r ,i) ,@z))))


(defun lassoc (x l eq)
  (declare (list l))
  (assoc x l :test eq))

(defun lmember (x l eq)
  (declare (list l))
  (member x l :test eq))

(defun string-prefix? (s1 s2 i)
  (declare (string s1 s2) (integer i))
  (string= s1 s2 :end1 i :end2 i))

(defun string-position-right (c s)
  (declare (character c) (string s))
  (position c s :test #'char= :from-end t))

(defun basename (f)
  (let ((f (file-namestring (merge-pathnames
                             (make-pathname :type "x") f))))
    (subseq f 0 (- (length f) 2))))

(defun ignore2 (i ii)
  (declare (ignore i ii))
  (values))

;(scmxlate-rename
; (ignore2 #'ignore2)
; )
