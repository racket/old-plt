(module installer mzscheme
  (provide installer)

  (define (installer plthome)
    (let* ([java.io (build-path 
                    (collection-path "profj" "libs" "java" "io"))]
           [serial (build-path java.io "Serializable.jinfo")]
           [comp (build-path java.io "compiled" "Serializable.jinfo")])
      (cond
        ((and (file-exists? comp) (not (= (file-or-directory-modify-seconds comp)
                                          (file-or-directory-modify-seconds serial))))
         (delete-file comp)
         (copy-file serial comp))
        ((not (file-exists? comp)) (copy-file serial comp))))))
