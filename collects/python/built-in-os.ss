#cs(module built-in-os mzscheme
  (require "primitives.ss"
           "runtime-support.ss"
           (lib "file.ss")
           (lib "list.ss"))
  (provide listdir
           isdir
           isfile
           sort)

  (define (listdir path)
     (list->py-list%
      (map string->py-string%
           (directory-list (py-string%->string path)))))
     
  ;; these are not standard
     
  (define (isdir path)
    (bool->py-number% (directory-exists? (py-string%->string path))))
     
  (define (isfile path)
    (bool->py-number% (file-exists? (py-string%->string path))))
     
  (define (sort l)
    (list->py-list% (quicksort (py-sequence%->list l) py<)))
     
     
  )