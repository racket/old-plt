(defvar *cl-command-name* "lisp")


#+clisp
(setq *cl-command-name* "clisp -q")

#+clisp
(defun system (s)
  (shell s))

(if (probe-file "slatex.scm")
    (delete-file "slatex.scm"))

(rename-file "my-slatex-src.scm" "slatex.scm")



#+unix
(with-open-file (o "slatex" :direction :output
                   :if-exists :supersede)
  (format o "echo '~%")
  (format o "(load ~s)~%" (or *target-file* "slatex.scm"))
  (format o "(slatex::process-main-tex-file \"'$1'\")' | ~a~%"
          *cl-command-name*)
  (format o "if test -f pltexchk.jnk~%")
  (format o "then tex $1; rm pltexchk.jnk~%")
  (format o "else latex $1~%")
  (format o "fi~%"))

#+unix
(system "chmod +x slatex")

#+win32
(with-open-file (o "slatex.bat" :direction :output
                   :if-exists :supersede)
  (format o "@echo off~%")
  (format o "echo (load ~s) >> Zslatex.jnk~%"  *target-file*)
  (format o "echo (slatex::process-main-tex-file \"%1\") >> Zslatex.jnk~%")
  (format o "echo (load \"Zslatex.jnk\") | ~a~%" *cl-command-name*)
  (format o "del Zslatex.jnk~%")
  (format o "if exist pltexchk.jnk goto one~%")
  (format o "goto two~%")
  (format o ":one~%")
  (format o "call tex %1~%")
  (format o "del pltexchk.jnk~%")
  (format o "goto end~%")
  (format o ":two~%")
  (format o "call latex %1~%")
  (format o ":end~%"))

(with-open-file 
 (o "callsla.scm" :direction :output)
 (prin1 `(let ((already-loaded-p nil))
          (defun call-slatex (f)
             (unless already-loaded-p
               (load ,(or *target-file* "slatex.scm"))
               (setq already-loaded-p t))
             (slatex::process-main-tex-file f)
             (format t "Call (La)TeX on ~a now~%" f)
             )) o)
 (terpri o))
