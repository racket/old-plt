(module repository mzscheme

  (require "private/archives.ss"
           "private/planet-web-page.ss"
           "server-config.ss"
           "private/planet-shared.ss"
           (lib "list.ss")
           (lib "file.ss"))
  
  (define WEBROOT (make-parameter "/home/jacobm/html/planet-mockup/"))

  (define this-version (language-version->repository (version)))
  
  ; get-latest-version : string string -> (Nat | #f) (Nat | #f)
  (define (get-latest-version owner plt-file)

    (let ((path (build-path (PLANET-SERVER-REPOSITORY) this-version owner (file-name-from-path plt-file))))
      (if (not (directory-exists? path))
          (values #f #f)
          (let* ([major-versions (filter (lambda (x) x) (map string->number (directory-list path)))]
                 [latest-maj (apply max major-versions)]
                 [minor-versions (filter 
                                  (lambda (x) x)
                                  (map 
                                   string->number 
                                   (directory-list (build-path path (number->string latest-maj)))))]
                 [latest-min (apply max minor-versions)])
            (values latest-maj latest-min)))))

  (define (add-archive-to-repository owner plt-file minor-update?)
    
    (define package-name (file-name-from-path plt-file))
    (let-values ([(curr-maj curr-min) (get-latest-version owner package-name)])
      (let* ((directories-to-create
              (cond
                [(not curr-maj)
                 (list owner package-name "1" "0")]
                [minor-update?
                 (list owner package-name (number->string curr-maj) (number->string (add1 curr-min)))]
                [else
                 (list owner package-name (number->string (add1 curr-maj)) "0")]))
             (relative-path (lambda (base) (apply build-path base directories-to-create))))
        (begin
          (make-directory* (relative-path (build-path (PLANET-SERVER-REPOSITORY) this-version)))
          (copy-file plt-file (build-path (relative-path (build-path 
                                                          (PLANET-SERVER-REPOSITORY)
                                                          this-version))
                                          package-name))
          (make-directory* (relative-path (build-path (WEBROOT) (DOCS-DIR))))
          #;(extract-files-from-archive plt-file 
                                      (build-path (PLANET-SERVER-REPOSITORY) this-version owner package-name)
                                      #rx"planet\\.txt")
          (extract-files-from-archive plt-file 
                                      (build-path (PLANET-SERVER-REPOSITORY) this-version owner package-name)
                                      #rx"info\\.ss")
          (extract-files-from-archive plt-file 
                                      (relative-path (build-path (WEBROOT) (DOCS-DIR)))
                                      #rx"doc\\.txt")
          
          (build-web-page-file (WEBROOT))))))
  
  (provide add-archive-to-repository))