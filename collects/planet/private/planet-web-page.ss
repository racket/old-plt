(module planet-web-page mzscheme
  
  (require "planet-shared.ss"
           "planet-getinfo.ss"
           "../server-config.ss"
           (file "/home/jacobm/iplt/web/common/layout.ss")
           (lib "xml.ss" "xml")
           (lib "list.ss")
           (lib "file.ss"))
  
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
    (let ((tree (archive-as-tree webroot)))
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
       (tree->table tree)
       (make-tall-page-section "Available Packages: Detail")
       (tree->xexprs tree))))
  
  ; owner ::= (make-owner string (listof package))
  (define-struct owner (name packages))
  ; package ::= (make-package string string nat nat (listof xexpr) (string | #f) string
  (define-struct package (owner-name name maj min blurb doc.txt primary-file))
  
  ; pkg->anchor : package -> string
  ; gets an anchor link for the given package
  (define pkg->anchor package-name)
  
  
  ;; ------------------------------------------------------------
  ;; this should go elsewhere
  (define (current-repository-contents)
    (define id (lambda (x) x))
    (apply list
           (cdr
            (tree->list
             (filter-tree-by-pattern
              (directory->tree (planet-server-repository) 
                               (lambda (x) 
                                 (let ((filename (file-name-from-path x)))
                                   (not (memv filename (list "CVS" (metainfo-file)))))))
              (list id id id id string->number string->number))))))
  ;; ------------------------------------------------------------
  
  ;; archive-as-tree : string -> listof owner
  ;; produces a tree suitable for generating the web page
  (define (archive-as-tree webroot)
    
    ; owner-line->owner : list -> owner
    (define (owner-line->owner owner)
      (make-owner
       (owner->name owner)
       (quicksort
        (map 
         (lambda (x) (package-line->package x (owner->name owner)))
         (owner->packages owner))
        (lambda (a b) (string<? (package-name a) (package-name b))))))
    
    ; package-line->package : list string -> package
    (define (package-line->package pkg owner-name)
      
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
      
      (make-package owner-name (pkg->name pkg) latest-major-version latest-minor-version
                    description
                    (if (file-exists? (build-path webroot
                                                  (DOCS-DIR)
                                                  owner-name
                                                  (pkg->name pkg) 
                                                  (number->string latest-major-version)
                                                  (number->string latest-minor-version)
                                                  "doc.txt"))
                        (format "~a/~a/~a/~a/~a/doc.txt"
                                (docs-dir)
                                owner-name
                                (pkg->name pkg)
                                latest-major-version
                                latest-minor-version)
                        #f)
                    file-to-require))
    
    
    (let* ([owners/all-versions (current-repository-contents)]
           [owners (let ((x (assoc (version) owners/all-versions)))
                     (if x (cdr x) '()))])
      (quicksort
       (map 
        owner-line->owner
        owners)
       (lambda (a b) (string<? (owner-name a) (owner-name b))))))
  
  ;; tree->table : tree -> xexpr[html table]
  ;; builds a summary table of all the packages available.
  (define (tree->table tree)
    (define (owner->table-rows owner) (map package->table-row (owner-packages owner)))
    (define (package->table-row pkg)
      `(tr ((bgcolor "#ddddff")) 
           (td ((valign "top")) nbsp (a ((href ,(format "#~a" (pkg->anchor pkg)))) ,(package-name pkg)))
           (td ((valign "top")) ,@(package-blurb pkg))))
    
    `(table ((width "100%"))
            (tbody
             ,@(apply append (map owner->table-rows tree)))))
      
  
  (define (tree->xexprs tree)
    
    (define (owner->html owner)
      `(div 
        ((class "owner"))
        (h2 ,(owner-name owner))
        ,@(map package->html (owner-packages owner))))
    
    (define (package->html pkg)
      `(div ((class "package")
             (style "background-color: #f3f4ff; padding-left: 10px; margin-left: 10px; margin-right: 30px;"))
            (a ((name ,(pkg->anchor pkg))))
            (h3 ,(package-name pkg))
            (div ((class "latestVersion"))
                 ,(if (package-doc.txt pkg)
                      `(a ((href ,(package-doc.txt pkg)))
                          "documentation")
                      `(span ((class "noDocs")) "[no documentation available]"))
                 nbsp "-" nbsp
                 "latest version: " ,(format "~a.~a" (package-maj pkg) (package-min pkg)))
            (br)
            (tt ,(format "(require (planet ~s (~s ~s ~s ~s)))" 
                         (package-primary-file pkg)
                         (package-owner-name pkg)
                         (package-name pkg) 
                         (package-maj pkg)
                         (package-min pkg)))
            (p ,@(package-blurb pkg))))

      (map owner->html tree))
  
  
  
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