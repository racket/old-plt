(module planet-web-page mzscheme
  
  (require "../util.ss"
           (lib "xml.ss" "xml"))
  
  (define WEB-PAGE-FILE (make-parameter "index.html"))
  
  (define (build-web-page-file)
    (with-output-to-file (WEB-PAGE-FILE)
      (lambda () (write-xml/content (xexpr->xml (generate-web-page))))))
      
  
  ;; generate-web-page : -> xexpr[xhtml]
  ;; makes a web page telling all currently-available packages
  (define (generate-web-page)
    (let* ([packages/all-versions (current-repository-contents)]
           [packages (let ((x (assoc (version) packages/all-versions)))
                       (if x (cdr x) '()))])
      `(html (head (title "Available PLaneT Packages")
                   (style ((type "text/css"))
                          "@import style.css;"))
             (body
              (h1 "Currently available PLaneT packages:")
              ,@(map owner-line->html packages)))))
  
  (define (owner-line->html owner)
    `(div 
      ((class "owner"))
      (h2 ,(car owner))
      ,@(map (lambda (x) (package-line->html x (car owner))) (cdr owner))))
  
  (define (join l sep)
    (define the-list
      (let loop ((l l))
        (cond
          [(null? l) '()]
          [(null? (cdr l)) (list (car l))]
          [else (list* (car l) sep (loop (cdr l)))])))
    (apply string-append the-list))
  
  (define (package-line->html pkg owner-name)
    `(div ((class "package"))
          (h3 ,(car pkg))
          (table ((summary ,(string-append 
                             "Available versions: "
                             (join 
                              (apply 
                               append
                               (map 
                                (lambda (maj) (map (lambda (min) (format "~a.~a" (car maj) (car min))) (cdr maj)))
                                (cdr pkg)))
                              ", "))))
                 (thead (tr (td (b "Major version")) (td (b "Minor versions")) (td (b "To require")))
                 (tbody  
                  ,@(map 
                     (lambda (maj)
                       `(tr 
                         (td ,(number->string (car maj))) 
                         (td ,(join (map (lambda (x) (number->string (car x))) (cdr maj)) ", "))
                         (td (tt "(require (planet " 
                                 (it "[file]")
                                 " "
                                 ,(format "~s" `(,owner-name ,(car pkg) ,(car maj) 0))
                                 "))"))))
                     (cdr pkg))))))))