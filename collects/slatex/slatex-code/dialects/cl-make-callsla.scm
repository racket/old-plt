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

