":";exec clisp -q $0
":";exec lisp -Q -L $0

(defvar *scmxlate-version* "0L1")

;(setq *load-verbose* nil)

(defvar *using-scxmlate-p* t)

(load (merge-pathnames "scm2cl" *load-pathname*))

(defvar *compilep* nil) 
(defvar *shell-script-p* nil)
(defvar *names-defined* '())
;(defvar *names-defined-in-current-file* '())
(defvar *target-file* nil)
(defvar *source-is-scheme-p* nil)

(defvar *files-to-be-ported*
  (with-open-file (i "dialects/files-to-be-ported.scm"
                     :direction :input)
    (let ((r '()))
      (loop
       (let ((f (read i nil :eof-object)))
         (when (eql f :eof-object) (return))
         (push (string-downcase (symbol-name f)) r)))
      r)))

(defun copy-port-to-port (i o)
  (loop
   (let ((c (read-char i nil)))
     (unless c (return))
     (princ c o))))

(defun copy-file-to-port (f o)
  (with-open-file (i f :direction :input)
    (copy-port-to-port i o)))


(defun writeln (e o)
  (pprint
   (if *source-is-scheme-p*
       (let ((*scm2cl-bound-vars* '()))
         (scm2cl-sexp (nsublis *scm2cl-names* e)))
       e) o)
  (terpri o))

(defun names-defined (x)
  (let ((r '()))
    (labels ((aux (x) 
                  (when (and (consp x) (>= (length x) 2))
                    (case (car x)
                      ((defconstant define define-macro define-syntax
                                    defmacro defparameter defstruct 
                                    defun defvar)
                       (push
                        (let ((name (cadr x)))
                          (if (consp name) (car name) name))
                        r))
                      ((scmxlate-ignore)
                       (setf r (append (cdr x) r)))
                      ((scmxlate-rename-define)
                       (setf r (append (mapcar #'car (cdr x)) r)))
                      ((begin if when unless)
                       (dolist (x1 (cddr x))
                         (aux x1)))))))
      (aux x))
      r))

(defun translate-port-to-port (i o)
  (loop
   (let* ((x (read i nil :eof-object))
          (names (names-defined x))
          (name (car names))) 
     (when (eql x :eof-object) (return))
     (when (consp x)
       (let ((a (car x)))
         (cond ((and (not (member a '(scmxlate-ignore scmxlate-rename-define)))
                     (member name *names-defined*))
                ;(format t "ignoring ~a~%" name)
                ;already defined or unwanted definition
                nil)
               (t
                (if names
                    (setq *names-defined*
                          (append names *names-defined*)))
                (case a
                  ((scmxlate-rename scmxlate-rename-define)
                   (dolist (x-y (cdr x))
                     (let ((x (car x-y)))
                       ;(push x *names-defined-in-current-file*)
                       (push (cons x (cadr x-y)) *scm2cl-names*))))
                  ((eval-if-mzscheme scmxlate-ignore scmxlate-prefix)
                   nil)
                  ((scmxlate-include)
                   (translate-file-to-port
                    (concatenate 'string "dialects/"
                                 (cadr x)) o))
                  ((scmxlate-target-file)
                   (setq *target-file* (cadr x)))
                  ((scmxlate-compile?)
                   (setq *compilep* (cadr x)))
                  (t
                   (when (member a '(define defun))
                     (let ((name (cadr x)) (e1 (caddr x))
                           (proc-p nil))
                       (cond ((eql a 'defun) 
                              (setq proc-p t))
                             ((consp name)
                              (setq name (car name))
                              (setq proc-p t))
                             ((and (consp e1)
                                   (null (cdddr x))
                                  (or 
                                   (eq (car e1) 'lambda)
                                       (and (= (length e1) 3)
                                   (eql (car e1) 'let)
                                   (eql (car (caddr e1)) 'lambda))))
                              (setq proc-p t)))
                          (if proc-p
                              (push (cons name `(function ,name))
                                    *scm2cl-names*))))
                   (writeln x o))))))))))

(defun translate-file-to-port (f o)
    (with-open-file (i f :direction :input)
      (translate-port-to-port i o)))

(dolist (file-to-be-ported *files-to-be-ported*)
  (format t "Porting ~a...~%" file-to-be-ported)
  (let ((dialects-dir (merge-pathnames
                       (make-pathname :directory
                                      '(:relative "dialects"))
                       file-to-be-ported))
        (fname (pathname-name file-to-be-ported)))
    (setq *shell-script-p* nil)
    (setq *compilep* 'ask)
    (setq *names-defined* '())
    (let ((*scm2cl-names* *scm2cl-names*)
          (start-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "cl-start-" fname))
                     dialects-dir)))
             (and (probe-file f) f)))
          (preamble-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "cl-preamble-" fname))
                     dialects-dir)))
             (and (probe-file f) f)))
          (user-override-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "scmxlate-" fname))
                     file-to-be-ported)))
             (and (probe-file f) f)))
          (override-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "cl-procs-" fname))
                     dialects-dir)))
             (and (probe-file f) f)))
          (postamble-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "cl-postamble-" fname))
                     dialects-dir)))
             (and (probe-file f) f)))
          (finish-file
           (let ((f (merge-pathnames
                     (make-pathname :name
                                    (concatenate 'string "cl-finish-" fname))
                     dialects-dir)))
             (and (probe-file f) f)))
          (target-file
           (merge-pathnames
            (make-pathname :name
                           (concatenate 'string "my-" fname))
            file-to-be-ported)))
    (when (probe-file start-file) (load start-file))
    (with-open-file (o target-file :direction :output
                       :if-exists :supersede)
      (when preamble-file
        (with-open-file (i preamble-file :direction :input)
          ;what about shellmagic if compiling?
          (copy-port-to-port i o))
        (terpri o))
      (dolist (f (list user-override-file override-file))
        (when f
          (translate-file-to-port f o)))
      (with-open-file (i file-to-be-ported)
        (let ((c (peek-char nil i)))
          (when (member c '(#\# #\"))
            (setq *shell-script-p* t)
            (when (char= c #\#) (read-line i))))
        (format o "~%;Configured for Common Lisp by scmxlate, v ~a~%" *scmxlate-version*)
        (format o ";(c) Dorai Sitaram~%")
        (format o ";http://www.ccs.neu.edu/~~dorai/scmxlate/scmxlate.html~%")
        (let ((*source-is-scheme-p* t))
          (translate-port-to-port i o)))
      (terpri o)
      (when postamble-file
        (copy-file-to-port postamble-file o)
        (terpri o)))
      #+clisp
      (when (eql *compilep* 'ask) (format t "~&")
        (setq *compilep* (yes-or-no-p "Compile?"))) 
      (when *compilep*
        (let ((target-file-so
               (merge-pathnames
                (make-pathname :name
                               (concatenate 'string "my-compiled-" fname))
                file-to-be-ported)))
          (format t "~&Compiling ~a to ~a~%" target-file target-file-so)
          (format t "(If this fails, try without compile option.)~%")
          (compile-file target-file :output-file target-file-so)
          (with-open-file (o target-file :direction :output
                             :if-exists :supersede)
            (when preamble-file
              (with-open-file (i preamble-file :direction :input)
                (when (member (peek-char nil i) '(#\# #\"))
                  (princ (read-line i) o)
                  (terpri o))))
            (copy-file-to-port target-file-so o))))
      #+unix
      (let ((result-file (namestring target-file)))
        (when *shell-script-p*
          (shell (concatenate 'string "chmod +x " result-file)))
        (cond (finish-file (load finish-file))
              (t
               (format t "~&Resulting file is `~a'~%" result-file)
               (format t "You may want to rename it~%")))))))

(cond ((fboundp 'bye) (bye))
      ((fboundp 'exit) (exit))
      ((fboundp 'quit) (quit))
      (t (format t "~&You may exit CL now!~%")))

