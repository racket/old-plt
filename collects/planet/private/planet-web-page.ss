(module planet-web-page mzscheme
  
  (require "../util.ss"
           "../server-config.ss"
           (lib "xml.ss" "xml")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (provide build-web-page-file)
  
  (define WEB-PAGE-FILE (make-parameter "index.html"))
  (define DOCS-DIR (make-parameter "docs"))
  
  (provide WEB-PAGE-FILE DOCS-DIR)
  
  
  (define (build-web-page-file webroot)
    (with-output-to-file (build-path webroot (WEB-PAGE-FILE))
      (lambda () (write-xml/content (xexpr->xml (generate-web-page webroot))))
      'replace))
  
  ;; generate-web-page : -> xexpr[xhtml]
  ;; makes a web page telling all currently-available packages
  (define (generate-web-page webroot)
    
    (define (owner-line->html owner)
      `(div 
        ((class "owner"))
        (h2 ,(owner->name owner))
        ,@(map 
           (lambda (x) (package-line->html x (owner->name owner)))
           (quicksort 
            (owner->packages owner)
            (lambda (a b) (string<? (pkg->name a) (pkg->name b)))))))
    
    (define (package-line->html pkg owner-name)
      (define description (let ((metainfo (build-path (PLANET-SERVER-REPOSITORY) 
                                                      (version)
                                                      owner-name
                                                      (pkg->name pkg)
                                                      (METAINFO-FILE))))
                            (if (file-exists? metainfo)
                                (read-file-to-string metainfo)
                                "No description available.")))
      
      (define latest-major-version (apply max (pkg->major-versions pkg)))
      (define latest-minor-version (apply max (pkg->minor-versions pkg latest-major-version)))
      
      `(div ((class "package"))
            (h3 ,(pkg->name pkg))
            (div ((class "latestVersion"))
                 ,(if (file-exists? (build-path webroot
                                                (DOCS-DIR)
                                                owner-name
                                                (pkg->name pkg) 
                                                (number->string latest-major-version)
                                                (number->string latest-minor-version)
                                                "doc.txt"))
                      `(a ((href ,(format "~a/~a/~a/~a/~a/doc.txt"
                                          (docs-dir)
                                          owner-name
                                          (pkg->name pkg)
                                          latest-major-version
                                          latest-minor-version)))
                          "documentation")
                      `(span ((class "noDocs")) "[no documentation available]"))
                 (br)
                 "Latest major version: " ,(number->string latest-major-version)
                 (br)
                 "Latest minor version: " ,(number->string latest-minor-version)
                 (br)
                 "To require: " (tt ,(format "(require (planet [file] (~s ~s ~s ~s)))" owner-name (pkg->name pkg) latest-major-version latest-minor-version)))
            (p ,description)))
    
    
    (let* ([owners/all-versions (current-repository-contents)]
           [owners (let ((x (assoc (version) owners/all-versions)))
                     (if x (cdr x) '()))])
      `(html (head (title "Available PLaneT Packages")
                   (style ((type "text/css"))
                          "@import \"style.css\";"))
             (body
              (h1 "Currently available PLaneT packages")
              ,@(map 
                 owner-line->html
                 (quicksort owners (lambda (a b) (string<? (owner->name a) (owner->name b)))))))))
  
  
  
  ;; ============================================================
  ;; UTILITIES
  ;; ============================================================
  
  ;; ==============================
  ;; OWNER MANIPULATION
  ;; ==============================
  (define owner->name car)
  (define owner->packages cdr)
  
  ;; ==============================
  ;; PKG MANIPULATION
  ;; ==============================
  (define pkg->name car)
  
  (define (pkg->major-versions pkg)
    (map car (cdr pkg)))
  
  (define (pkg->minor-versions pkg maj)
    (map car (cdr (assq maj (cdr pkg)))))        
  
  
  ;; join : (listof string) string -> string
  ;; joins the given strings with the given separator between each
  (define (join l sep)
    (define the-list
      (let loop ((l l))
        (cond
          [(null? l) '()]
          [(null? (cdr l)) (list (car l))]
          [else (list* (car l) sep (loop (cdr l)))])))
    (apply string-append the-list))
  
  ;; read-file-to-string : path -> string
  ;; reads the contents of the given file into a string. 
  ;; (That's a bad idea for large files, but for small ones it's fine.)
  (define (read-file-to-string path)
    (with-input-from-file path
      (lambda ()
        (let loop ((acc '()))
          (let ((line (read-line)))
            (cond
              [(eof-object? line)
               (apply string-append (reverse acc))]
              [else
               (loop (cons line acc))])))))))