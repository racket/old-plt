(module pre-installer mzscheme
  (require 
   (lib "file.ss")
   (lib "build-from-source.ss" "plot"))

  (define (pre-installer plthome)      
    (let ([dir (build-path (collection-path "plot")
                           "precompiled"
                           "native"
                           (system-library-subpath #f))])
	(if (directory-exists? dir) ; just copy things over
            (let ((compiled-dir (build-path
                                 (collection-path "plot"
                                                  "compiled"
                                                  "native"
                                                  (system-library-subpath)))))
              (when (directory-exists? compiled-dir)
                (delete-directory/files compiled-dir))
              (make-directory* compiled-dir)
              (copy-directory/files dir compiled-dir))

            (build))))
                 
  (provide pre-installer))
