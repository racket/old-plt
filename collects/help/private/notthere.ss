(module notthere mzscheme
  (require (lib "xml.ss" "xml"))
  
  (provide build-notthere-page)
  
  ;; build-notthere-page : string string string -> port
  ;; constructs a port that contains the missing document html page.
  (define (build-notthere-page missing-doc-name missing-html-file missing-html-url)
    (let ([str-out (open-output-string)])
      (write-xml/content (xexpr->xml (construct-web-page missing-doc-name missing-html-file missing-html-url)) str-out)
      (open-input-string (get-output-string str-out))))

  ;; construct-web-page : string string string -> xexpr
  ;; builds the contents of the page to show the user.
  (define (construct-web-page missing-doc-name missing-html-file missing-html-url)
    `(html
      (title "Documentation Missing")
      (b (font ([color "red"]) (h1 "DOCUMENTATION MISSING")))
      (p
       "You tried to access documentation for"
       (b ,missing-doc-name))
      
      (p
       "The documentation is not installed on this machine,"
       "probably because it is not part of the standard DrScheme distribution.")
      
      (p
       ,@(let* ([subpath (if (directory-exists? (build-path (collection-path "help") "CVS"))
                             "PreRelease/DocBundles200"
                             (format "packages/~a/DocBundles" (version)))]
                [url (format "http://www.cs.rice.edu/CS/PLT/~a/~a-doc.plt" subpath missing-doc-name)]
                [web-url (format "http://www.cs.rice.edu/CS/PLT/packages/~a/doc/~a/~a"
                                 (version) missing-doc-name missing-html-file)])
           (list
            `(h3 "To Read the Documentation Online")
            `(UL
              (LI (A ([href ,web-url]) ,web-url)))
            `(P
              (H3 "To Download and Install the Documentation")
              (UL (LI (A ((href ,url)) ,url)))
              "Follow the above link within Help Desk to download the"
              "documentation. Help Desk will install it automatically."
              "After installing, "
              (a ((HREF ,missing-html-url)) "continue")
              " to the newly-installed documentation. ")
            `(p
              "If you use a proxy server, this download will not "
              "work. Instead, use some other proxy-based mechanism"
              "to download the .plt file from this url:")
            `(p (center ,url))
            `(p
              "and then use the Open URL menu item from the"
              "File menu to open that .plt from your local computer."
              "(Alternatively, you can drag the .plt file onto Setup PLT,"
              "or start up Setup PLT with the .plt file from the command line,"
              "depending on your platform.)")))))))


