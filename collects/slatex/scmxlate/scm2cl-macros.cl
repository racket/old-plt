;translator macros

(defvar *scm2cl-macros* (make-hash-table))

(defvar *scm2cl-gentemp-prefix* "Scheme-to-CL-")

(defmacro defmacro-scm2cl (name args &rest body)
  `(setf (gethash ',name *scm2cl-macros*)
     #'(lambda ,args ,@body)))

(defmacro-scm2cl scm2cl-lambda (args &rest body)
                  `(function
                    (lambda ,(scm2cl-dot-to-rest args) ,@body)))

(defmacro-scm2cl scm2cl-functionless-lambda (args &rest body)
  `(lambda ,(scm2cl-dot-to-rest args) ,@body))

(defun scm2cl-dot-to-rest (vv)
  (if (null vv) nil
    (if (symbolp vv) `(&rest ,vv)
      (let* ((l (last vv))
              (d (cdr l)))
        (unless (null d)
          (setf (cdr l) `(&rest ,d)))
        vv))))

(defmacro-scm2cl define (x &rest ee)
  (let ((e (car ee)))
    (cond ((and (consp x) (eq (car x) 'function))
           (setq x (cadr x)))
          ;remaining clauses add definee to
          ;*scm2cl-names*
          (*using-scmxlate-p*
           ;already taken care of
           nil) 
          ((consp x)
           (let ((f (car x)))
             (push (cons f `(function ,f)) *scm2cl-names*)))
          ((and (consp e) (eq (car e) 'scm2cl-lambda))
           (push (cons x `(function ,x)) *scm2cl-names*))
          ((and (consp e) (= (length e) 3)
                (eq (car e) 'scm2cl-let) (eq (car (caddr e)) 'scm2cl-lambda))
           (push (cons x `(function ,x)) *scm2cl-names*)))
    ;
    (if (and (symbolp x) (cdr ee))
        (error "bad define: ~s" (cons 'define (cons x ee))))
    ;
    (cond ((and (symbolp x) (consp e) (eq (car e) 'scm2cl-lambda))
           `(defun ,x ,(scm2cl-dot-to-rest (cadr e)) ,@(cddr e)))
          ((and (symbolp x) (consp e) (= (length e) 3)
                (eq (car e) 'scm2cl-let) 
                (eq (car (caddr e)) 'scm2cl-lambda))
           `(scm2cl-let ,(cadr e)
              (define ,x ,(caddr e))))  
          ((consp x)
           `(defun ,(car x) ,(scm2cl-dot-to-rest (cdr x)) ,@ee))
          (t `(defparameter ,x ,e)))))

(defmacro-scm2cl scm2cl-let (n &rest ee)
  (if (and n (symbolp n))
    (let ((tail-recursive-p (search "LOOP" (symbol-name n))))
      `(,(if (and tail-recursive-p (= tail-recursive-p 0))
           'scm2cl-named-let-tr
           'scm2cl-named-let-non-tr) ,n ,@ee))
    `(let ,n ,@ee)))

(defmacro-scm2cl scm2cl-let* (vv &rest ee)
  (if (<= (length vv) 1)
      `(let ,vv ,@ee)
    `(let (,(car vv)) (scm2cl-let* ,(cdr vv) ,@ee))))

(defmacro-scm2cl scm2cl-named-let-non-tr (n xvxv &rest ee)
  `(labels ((,n ,(mapcar #'car xvxv) ,@ee))
     (,n ,@(mapcar #'cadr xvxv))))

(defmacro-scm2cl scm2cl-named-let-tr (n xvxv &rest ee)
  (let ((xx (mapcar #'car xvxv)))
    `(let ,xvxv
       (flet ((,n ,xx
		  (throw ',n (values ,@xx))))
	 (loop
	  (multiple-value-setq ,xx
	    (let ,(mapcar #'(lambda (x) `(,x ,x)) xx)
	      (catch ',n
		(return ,(if (= (length ee) 1) (car ee)
			   (cons 'progn ee)))))))))))

(defmacro-scm2cl scm2cl-cond (&rest clauses)
  (case (length clauses)
    ((0) 'nil)
    ((1) (let* ((clause (car clauses))
                (n (length clause))
                (test (car clause)))
           (if (eq test 'else) (setq test 't))
           (cond ((= n 1) `,test)
                 ((= n 2) `(if ,test ,(cadr clause)))
                 ((and (= n 3) (eq (cadr clause) '=>))
                  `(let ((__test__ ,test))
                     (if __test__ (funcall ,(caddr clause) __test__))))
                 (t `(when ,test ,@(cdr clause))))))
    (t (let* ((clause (car clauses))
              (n (length clause)))
         (cond ((= n 1)
                `(or ,(car clause) (scm2cl-cond ,@(cdr clauses))))
               ((= n 2)
                `(if ,(car clause) ,(cadr clause)
                   (scm2cl-cond ,@(cdr clauses))))
               ((and (= n 3) (eq (cadr clause) '=>))
                `(let ((__test__ ,(car clause)))
                   (if __test__ (funcall ,(caddr clause) __test__)
                     (scm2cl-cond ,@(cdr clauses)))))
               (t `(if ,(car clause)
                       (progn ,@(cdr clause))
                     (scm2cl-cond ,@(cdr clauses)))))))))

(defmacro-scm2cl scheme-defstruct (name &rest fields)
  `(defstruct (,name (:conc-name
                      ,(intern (concatenate 'string (symbol-name name) "."))))
     ,@fields))

(defun struct-setter-p (x)
  (and x (symbolp x)
       (let ((xs (symbol-name x)))
         (and (> (length xs) 4)
              (eql (search "SET!" xs) 0)))))

(defun struct-getter (setter-name)
  (intern (subseq (symbol-name setter-name) 4)))

(defun struct-maker-p (call)
  (and (oddp (length call))
       (let ((fn (car call)))
         (and (symbolp fn)
              (let ((fns (symbol-name fn)))
                (and (> (length fns) 5)
                     (eql (search "MAKE-" fns) 0)
                     (progn (pop call)
                            (loop
                             (if (null call) (return t))
                             (let ((f (pop call)))
                               (unless (and (consp f)
                                            (= (length f) 2)
                                            (eq (car f) 'quote))
                                 (return nil))
                               (pop call))))))))))

(defun keywordize-fieldnames (args)
  (let ((s '()))
    (loop
     (when (null args) (return))
     (push (intern (symbol-name (cadr (pop args)))
                   :keyword) s)
     (push (pop args) s)
     )
    (nreverse s)))

'(defmacro-scm2cl scheme-define-macro (name xfmr)
  ;MzScheme's define-macro
  (unless (eq (car xfmr) 'lambda) 
    (error "scheme-define-macro ~s ~s" name xfmr))
  `(defmacro ,name ,@(cdr xfmr)))

;mbe

(defmacro-scm2cl scheme-define-syntax (macroname synrules)
  (let ((keywords (cadr synrules))
        (clauses (cddr synrules)))
    `(defmacro ,macroname (&rest __syntax-rules-arg__)
       ,(scheme-mbe-syntax-rules-proc macroname keywords clauses
                                     '__syntax-rules-arg__
                                     '__syntax-rules-keywords__))))

(defmacro-scm2cl scheme-letrec-syntax (synruledefs &rest body)
  `(macrolet
    ,(mapcar #'(lambda (synruledef)
                 (let ((macroname (car synruledef))
                       (keywords (cadadr synruledef))
                       (clauses (cddadr synruledef)))
                   `(,macroname (&rest __syntax-rules-arg__)
                                ,(scheme-mbe-syntax-rules-proc macroname
                                                              keywords
                                                              clauses
                                                              '__syntax-rules-arg__
                                                              '__syntax-rules-keywords__))))
             synruledefs)
    ,@body))

;Actually, CL can't distinguish let- from letrec-syntax very well.

(defmacro-scm2cl scheme-let-syntax (synruledefs &rest body)
  (case (length synruledefs)
        ((0) `(progn ,@body))
        ((1) `(letrec-syntax ,synruledefs ,@body))
        (otherwise
         `(letrec-syntax (,(car synruledefs))
                         (let-syntax ,(cdr synruledefs)
                                     ,@body)))))

;;don't load mbe.cl if not available.  this feature needed
;;because scm2cl is used to create mbe.cl from mbe.scm

'(load (merge-pathnames "mbe" *load-pathname*)
      :if-does-not-exist nil)

;inline some scheme procedure calls

(defmacro-scm2cl scheme-assoc (x l)
  `(assoc ,x ,l :test #'equal))

(defmacro-scm2cl scheme-boolean? (o)
  `(case ,o
     ((t nil) t)
     (t nil)))

(defmacro-scm2cl scheme-current-input-port ()
  `*standard-input*)

(defmacro-scm2cl scheme-current-output-port ()
  `*standard-output*)

(defmacro-scm2cl scheme-call-with-input-file (f p)
  (if (and (consp p) (eq (car p) 'scm2cl-lambda))
      `(with-open-file (,(caadr p) ,f :direction :input)
                       ,@(cddr p))
    (let ((i (gentemp *scm2cl-gentemp-prefix*)))
      `(with-open-file (,i ,f :direction :input)
                       (funcall ,p ,i)))))

(defmacro-scm2cl scheme-call-with-output-file (f p)
  (if (and (consp p) (eq (car p) 'scm2cl-lambda))
      `(with-open-file (,(caadr p) ,f :direction :output)
                       ,@(cddr p))
    (let ((o (gentemp *scm2cl-gentemp-prefix*)))
      `(with-open-file (,o ,f :direction :output)
                       (funcall ,p ,o)))))

(defmacro-scm2cl scheme-char-whitespace? (c)
  (let ((c1 (gentemp *scm2cl-gentemp-prefix*)))
    `(let ((,c1 ,c))
       (or (char= ,c1 #\space) (char= ,c1 #\tab)
           (not (graphic-char-p ,c1))))))

(defmacro-scm2cl scheme-eof-object? (v)
  `(eq ,v :eof-object))

(defmacro-scm2cl scheme-list->string (l)
  `(concatenate 'string ,l))

(defmacro-scm2cl scheme-list->vector (l)
  `(concatenate 'vector l))

(defmacro-scm2cl scheme-make-string (n &optional c)
  `(make-string ,n :initial-element
                ,(if c c #\space)))

(defmacro-scm2cl scheme-make-vector (n &optional x)
  `(make-string ,n :initial-element ,x))

(defmacro-scm2cl scheme-member (x l)
  `(member ,x ,l :test #'equal))

(defmacro-scm2cl scheme-not-quite-call/cc (p)
  (let ((k (gentemp *scm2cl-gentemp-prefix*)))
    ;had to use gentemp above because we're writing
    ;expansion into file
    `(let ((,k (gensym)))
       (catch ,k
         ,(if (and (consp p) (eq (car p) 'scm2cl-lambda))
              `(,p #'(lambda (v) (throw ,k v)))
            `(funcall ,p #'(lambda (v) (throw ,k v))))))))

(defmacro-scm2cl scheme-number->string (n &optional b)
  `(write-to-string ,n
                    ,@(if b `(:base ,b) '())))

(defmacro-scm2cl scheme-open-input-file (f)
  `(open ,f :direction :input))

(defmacro-scm2cl scheme-open-output-file (f)
  `(open ,f :direction :output))

(defmacro-scm2cl scheme-peek-char (&optional p)
  `(peek-char nil ,p nil :eof-object))

(defmacro-scm2cl scheme-read (&optional p)
  `(read ,p nil :eof-object))

(defmacro-scm2cl scheme-read-char (&optional p)
  `(read-char ,p nil :eof-object))

(defmacro-scm2cl scheme-string (&rest cc)
  `(concatenate 'string (list ,@cc)))

(defmacro-scm2cl scheme-string-append (&rest ss)
  `(concatenate 'string ,@ss))

(defmacro-scm2cl scheme-string-set! (s i c)
  `(setf (char ,s ,i) ,c))

(defmacro-scm2cl scheme-string->list (s)
  `(concatenate 'list ,s))

(defmacro-scm2cl scheme-string->number (s &optional b)
  (let ((s1 (gentemp *scm2cl-gentemp-prefix*))
        (n (gentemp *scm2cl-gentemp-prefix*)))
    (if b
        `(let ((,s1 ,s))
           (if (position #\: ,s1 :test #'char=) nil
             (let* ((*read-base* ,b)
                    (,n (read-from-string ,s1 nil)))
               (if (numberp ,n) ,n nil))))
      `(let ((,s1 ,s))
         (if (position #\: ,s1 :test #'char=) nil
           (let ((,n (read-from-string ,s1 nil)))
             (if (numberp ,n) ,n nil)))))))

(defmacro-scm2cl scheme-string->symbol (s)
  (let ((s1 (gentemp *scm2cl-gentemp-prefix*)))
    `(let ((,s1 (map 'string
                  #'(lambda (c) (cond ((upper-case-p c) (char-downcase c))
                                      ((lower-case-p c) (char-upcase c))
                                      (t c))) ,s)))
       (if (or (string= ,s1 "") (not (char= (char ,s1 0) #\:)))
           (intern ,s1)
           (intern (subseq ,s1 1) :keyword)))))

(defmacro-scm2cl scheme-symbol? (x)
  (let ((x1 (gentemp *scm2cl-gentemp-prefix*)))
    `(let ((,x1 ,x))
       (and (symbolp ,x1)
            (not (or (eq ,x1 t) (eq ,x1 nil)))))))

(defmacro-scm2cl scheme-symbol->string (x)
  `(string-downcase (symbol-name ,x)))

(defmacro-scm2cl scheme-vector-set! (v i x)
  `(setf (svref ,v ,i) ,x))

(defmacro-scm2cl scheme-vector->list (v)
  `(concatenate 'list ,v))

(defmacro-scm2cl scheme-with-input-from-file (f th)
  `(with-open-file (__temp-input-port ,f :direction :input)
     (let ((*standard-input* __temp-input-port))
       (funcall ,th))))

(defmacro-scm2cl scheme-with-output-to-file (f th)
  `(with-open-file (__temp-output-port ,f :direction :output)
     (let ((*standard-output* __temp-output-port))
       (funcall ,th))))

;slib 

(defmacro-scm2cl scheme-call-with-input-string (s p)
  (if (and (consp p) (eq (car p) 'scm2cl-lambda))
      `(with-input-from-string (,(caadr p) ,s)
                       ,@(cddr p))
    (let ((i (gentemp *scm2cl-gentemp-prefix*)))
      `(with-input-from-string (,i ,s)
                       (funcall ,p ,i)))))

(defmacro-scm2cl scheme-call-with-output-string (s p)
  (if (and (consp p) (eq (car p) 'scm2cl-lambda))
      `(with-output-to-string (,(caadr p) ,s)
                       ,@(cddr p))
    (let ((o (gentemp *scm2cl-gentemp-prefix*)))
      `(with-output-to-string (,o ,s)
                       (funcall ,p ,o)))))

(defmacro-scm2cl scheme-load-relative (f)
  `(load (merge-pathnames (make-pathname :type ,*scm2cl-lisp-extension*)
           (merge-pathnames ,f *load-pathname*))))

(defmacro-scm2cl scheme-read-line (&optional i)
  `(read-line ,i nil :eof-object))

(defmacro-scm2cl scheme-string-index (s c)
  `(position ,c ,s :test #'char=))

(defmacro-scm2cl scheme-string-reverse-index (s c)
  `(position ,c ,s :test #'char= :from-end t))

;eof
