(module planet-web-page mzscheme
  
  (require "../util.ss"
           "planet-getinfo.ss"
           "../server-config.ss"
           (file "/usr/local/iplt/web/common/layout.ss")
           (lib "xml.ss" "xml")
           (lib "list.ss"))
  
  (provide build-web-page-file)
  
  (define WEB-PAGE-FILE (make-parameter "index.html"))
  (define DOCS-DIR (make-parameter "docs"))
  
  (provide WEB-PAGE-FILE DOCS-DIR)
  
  
  (define (build-web-page-file webroot)
    (parameterize ((current-write-page 
                    (lambda (x y) 
                      (let ((out (open-output-file
                                  (build-path webroot (WEB-PAGE-FILE))
                                  'replace)))
                        (write-xml/content (xexpr->xml y) out)
                        (close-output-port out)))))
      (write-tall-page 
       "http://planet.plt-scheme.org/"
       "PLaneT Package Repository"
       (generate-web-page webroot)
       (list (make-hubs-panel #f #f)))))
  
  ;; generate-web-page : -> listof xexpr[xhtml]
  ;; makes the body of a web page telling all currently-available packages
  (define (generate-web-page webroot)
    (list*
     `(div 
       ((class "description"))
       (p (strong "PLaneT") " is PLT Scheme's centralized package distribution system. Here you"
          " will find user-contributed Scheme packages along with instructions for using them.")
       (p "The packages on this site are user-contributed and not part of PLT Scheme. Be aware "
          "that when you download one of them for use in your programs, you are installing "
          "software on your computer that could deliberately or accidentally harm your system. "
          "Do not require from PLaneT any packages you do not trust.")
       (p "For more about how to use PLaneT and for instructions on turning your own code into"
          " packages, look up PLaneT in the DrScheme Help Desk."))
     (make-tall-page-section "Available Packages")
     (packages->xexprs webroot)))
  
  (define (packages->xexprs webroot)
    
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
      
      (define metainfo-file-path (build-path (PLANET-SERVER-REPOSITORY) 
                                             (version)
                                             owner-name
                                             (pkg->name pkg)
                                             (METAINFO-FILE)))
      
      (define metainfo
        (with-handlers ([not-break-exn? (lambda (e) (lambda (x y) (y)))])
          (let ((getter (get-info-from-file metainfo-file-path)))
            (if getter
                (lambda (item default) 
                  (with-handlers ([not-break-exn? (lambda (x) (default))])
                    (getter item)))
                (lambda (x default) (default))))))
      
      (define description 
        (let ((orig-blurb (metainfo 'blurb (lambda () "No description available."))))
          (if (string? orig-blurb)
              (list orig-blurb)
              orig-blurb)))
      
      (define file-to-require (metainfo 'primary-file (lambda () "[file]")))
      
      (define latest-major-version (apply max (pkg->major-versions pkg)))
      (define latest-minor-version (apply max (pkg->minor-versions pkg latest-major-version)))
      
      `(div ((class "package")
             (style "background-color: #f3f4ff; padding-left: 10px; margin-left: 10px; margin-right: 30px;"))
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
                 'nbsp 'sdot 'nbsp
                 "latest version: " ,(format "~a.~a" 
                                             (number->string latest-major-version)
                                             (number->string latest-minor-version))

                 (br)
                 (tt ,(format "(require (planet ~s (~s ~s ~s ~s)))" file-to-require owner-name (pkg->name pkg) latest-major-version latest-minor-version)))
            (p ,@description)))
    
    
    (let* ([owners/all-versions (current-repository-contents)]
           [owners (let ((x (assoc (version) owners/all-versions)))
                     (if x (cdr x) '()))])
      (map 
       owner-line->html
       (quicksort owners (lambda (a b) (string<? (owner->name a) (owner->name b)))))))
  
  
  
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