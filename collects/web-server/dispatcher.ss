(module dispatcher mzscheme
  (require "configuration-table-structs.ss"
           "util.ss"
           (lib "13.ss" "srfi")
           (lib "list.ss"))
  
  (provide (struct resource (path base))
           (struct static-resource ())
           (struct dynamic-resource ())
           make-dispatcher
           build-path-unless-absolute)
  
  ;; resource ::= static-resource | dynamic-resource
  (define-struct resource (path base))
  (define-struct (static-resource resource) ())
  (define-struct (dynamic-resource resource) ())
  
  ; build-path-unless-absolute : str str -> str
  (define (build-path-unless-absolute base path)
    (if (absolute-path? path)
        path
        (build-path base path)))
  
  
  
  ;; make-dispatcher: paths -> path -> resource
  ;; given a paths structure, make a function that consumes a url-path and produces a resource.
  (define (make-dispatcher paths)
    (check-duplicates (paths-htdocs paths)
                      (check-duplicates (paths-servlet paths)
                                        (list "/conf/refresh-servlets"
                                              "/conf/refresh-passwords"
                                              "/conf")))
    (let ([servlet-patterns
           (map
            (lambda (svt-path)
              (make-pattern-ex svt-path (paths-host-base paths) make-dynamic-resource-constructor))
            (paths-servlet paths))]
          [htdocs-patterns
           (map
            (lambda (htd-path)
              (make-pattern-ex htd-path (paths-host-base paths) make-static-resource-constructor))
            (paths-htdocs paths))])
      (make-matcher
       (sort-patterns (append servlet-patterns htdocs-patterns)))))
  
  
  ;; A simple pattern matching "language" for URL mapping
  ;;;; pattern ::= (path-prefix . path)
  ;;;;;; path ::= directory-path | file-path
  ;;;;;; directory-path ::= string
  ;;;;;; file-path ::= string
  ;;;;;; path-prefix ::= string
  ;;
  ;; SEMANTICS: A path is a string that leads to a directory or file in the server's file system.
  ;;            A path-prefix is a fragment of a path intended to match the first part of a URL's path.
  ;;
  ;;            For the (path-prefix . path) form the path portion is turned into an absolute path by calling
  ;;            build-path-unless-absolute. The rule is that if it is already an absolute-path, then don't
  ;;            mess with it. Otherwise, use build path, where the base path is paths-host-base field of the
  ;;            paths structure.
  ;;            E.g.: If paths-host-base = /foo/bar/baz then
  ;;            (/fiddle/sticks . bing/bang) |--> (/fiddle/sticks . /foo/bar/baz/bing/bang)
  ;;            (/fiddle/sticks . /bing/bang) |--> (/fiddle/sticks . /bing/bang)
  ;;
  ;;            directory-path vs. file-path
  ;;            If the path leads to a specific file, then it is a file-path, otherwise it is a directory-path
  
  ;; expand-pattern: pattern string -> (path-prefix . absolute-path)
  ;; expand a pattern according to the above
  (define (expand-pattern pat base)
    (cons (car pat)
          (simplify-path (build-path-unless-absolute base (cdr pat)))))
  
  ;; check-duplicates: (listof pattern) (listof string) -> (listof string)
  ;; add the strings from the first list to the second, checking for duplicates
  (define (check-duplicates from to)
    (cond
      [(null? from) to]
      [else
       (check-duplicates
        (cdr from)
        (check-duplicate (caar from) to))]))
  
  (define (check-duplicate str to)
    (cond
      [(null? to) (list str)]
      [(string=? str (car to)) (error "duplicate path: " str)]
      [else (cons (car to) (check-duplicate str (cdr to)))]))
  
  ;;;; pattern-ex ::= (path-prefix . (string -> resource))
  ;; SEMANTICS: The path-prefix is as above. The function consumes the part of the URL path
  ;;            that is not matched and produces a data structure. The structure will typically
  ;;            be either a static-resource or dynamic-resource
  
  ;; make-static-resource-constructor: string string -> string -> static-resource
  ;; a curried make-static-resource
  (define (make-static-resource-constructor url-path-prefix pattern-base)
    (lambda (url-path-suffix)
      (make-static-resource url-path-suffix pattern-base)))
  
  ;; make-dynamic-resource-constructor: string string -> string -> static-resource
  ;; a curried make-dynamic-resource
  (define (make-dynamic-resource-constructor url-path-prefix pattern-base)
    (cond
      [(file-exists? pattern-base)
       (let ([resource-map 
              (parameterize ([current-directory (directory-part pattern-base)])
                (dynamic-require `(file ,pattern-base) 'resource-map))])
         (lambda (url-path-suffix)
           (resource-map url-path-prefix url-path-suffix)))]
      [else
       (lambda (url-path-suffix)
         (make-dynamic-resource url-path-suffix pattern-base))]))
       
  
  ;; make-pattern-ex: pattern string (string string -> string -> resource) -> pattern-ex
  (define (make-pattern-ex svt-path base make-res-ctor)
    (let* ([expanded-pattern (expand-pattern svt-path base)]
           [url-path-prefix (car expanded-pattern)]
           [pattern-base (cdr expanded-pattern)])
      (cons (car expanded-pattern)
            (make-res-ctor url-path-prefix pattern-base))))
  
  ;; make-matcher: (listof pattern-ex) -> string -> (union resource #f)
  ;; take a (listof pattern-ex) and make a function that matches URL paths and produces the
  ;; propper resource.
  (define (make-matcher pats)
    (lambda (str)
      (let loop ([pats
                  (map
                   (lambda (pat)
                     (cons (regexp (string-append "^" (add-escapes (car pat)) "(.*)"))
                           (cdr pat)))
                   pats)])
        (cond
          [(null? pats) #f]
          [(regexp-match (caar pats) str)
           => (lambda (mtch)
                ((cdar pats) (cadr mtch)))]
          [else (loop (cdr pats))]))))
  
  ;; add-escapes: string -> string
  ;; escape all the special characters in a string so that they won't be mistaken as part of a regexp
  ;; heck, just escape *all* the characters.
  (define (add-escapes str)
    (list->string
     (foldr
      (lambda (char acc)
        (cons #\\ (cons char acc)))
      '()
      (string->list str))))
  
  ;; sort-patterns: (listof (cons string proc)) -> (listof (cons string proc))
  (define (sort-patterns pats)
    (quicksort pats
               (lambda (pat1 pat2)
                 (let ([str1 (car pat1)]
                       [str2 (car pat2)])
                   (not (string< str1 str2))))))
  )
