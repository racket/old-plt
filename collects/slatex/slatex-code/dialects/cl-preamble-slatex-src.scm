(unless (find-package :slatex)
  (make-package :slatex))

(in-package :slatex)

(defvar *operating-system*
  #+win32 'windows
  #-win32 'unix)
  

