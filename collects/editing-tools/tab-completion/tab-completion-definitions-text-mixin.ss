(module tab-completion-definitions-text-mixin mzscheme
  (provide tab-completion-definitions-text-mixin)
  (require (lib "compiler.ss" "compiler")
           (lib "cm.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "file.ss")
           (lib "etc.ss")
           
           "../common/drscheme-tool-support.ss"
           "../common/expansion-tool-definitions-text-mixin.ss"
           )

 #| parse-compiled is not necessary as of v299.30
    Thanks, Matthew!
  (require (lib "parse-compiled.ss" "parse-compiled"))
  (define (module-compiled-exports/all compiled-module)
    (match (parse-compiled compiled-module)
      [(list 'compilation-top max-let-depth resolve-prefix sxp)
       (match sxp
         [(list 'syntax index data)
          (match data
            [(list 'module-code requires requires-for-stx provides)
	     (match provides
               [(list 'provides (list ids ...))
                ids])])])]))
  |#

  (define (module-compiled-exports/all compiled-module)
    (let-values ([(own-provides var-provides)
		  (module-compiled-exports compiled-module)])
      (append own-provides var-provides)))
  
  
  (define (require-spec->provides spec)
    (define (zo-path->provides zo-path)
      (and zo-path
           (file-exists? zo-path)
           ;; TODO: It would be nice to not need the 'parse-compiled' C code.
           (module-compiled-exports/all (parameterize ([read-accept-compiled #t])
                              (with-input-from-file zo-path read)))))
    (define (file-path->zo-path path)
      (define-values (base f dir?) (split-path path))
      (define zo-path (build-path base "compiled"
                                  (regexp-replace #rx"\\.[^\\.]+"
                                                  (path->string f)
                                                  ".zo")))
      (and (file-exists? zo-path)
           zo-path))
    (define (path-string->zo-path ps)
      (file-path->zo-path (normalize-path ps)))
    (define (lib-path->zo-path lib collect-paths)
      (define base (apply collection-path (if (null? collect-paths)
                                              (list "mzlib")
                                              collect-paths)))
      (define zo-path (build-path base "compiled" (regexp-replace #rx"\\.[^\\.]+" lib ".zo")))
      (and (file-exists? zo-path)
           zo-path))
    (define (module-name-symbol->provides name)
      (define ns (make-namespace 'empty))
      ;; if the module doesn't exist, just skip it
      (with-handlers ([void (lambda _ null)])
        (namespace-attach-module (current-namespace) name ns)
        (parameterize ([current-namespace ns])
          (namespace-require name))
        (namespace-mapped-symbols ns)))

    (match spec
      [(list 'file path)
       (zo-path->provides (file-path->zo-path path))]
      [(list-rest 'lib library subcollects)
       (zo-path->provides (lib-path->zo-path library subcollects))]
      [(list-rest 'all-except spec excepts)
       (filter (lambda (id)
                 (not (member id excepts)))
               (require-spec->provides spec))]
      [(list 'rename spec old-name new-name)
       (map (lambda (id)
              (if (eq? id old-name)
                  new-name
                  id))
            (require-spec->provides spec))]
      [(list 'prefix prefix spec)
       (map (lambda (id)
              (string->symbol (format "~a~a" prefix id)))
            (require-spec->provides spec))]
      [spec (if (string? spec)
                (zo-path->provides (path-string->zo-path spec))
                (module-name-symbol->provides spec))]))
  
  (define (program-toplevel-symbols program-expansion)
    (define program-sxp (if (syntax? program-expansion)
                            (syntax-object->datum program-expansion)
                            program-expansion))
    (match program-sxp
      [(list 'module module-name language body)
       (program-toplevel-symbols body)]
      [(list '#%plain-module-begin commands ...)
       (apply append (map program-toplevel-symbols commands))]
      [(list 'begin exprs ...)
       (apply append (map program-toplevel-symbols exprs))]
      [(list 'define-values (list ids ...) vs) ids]
      [(list 'define-syntaxes (list ids ...) vs) ids]
      [(list (or 'require 'require-for-syntax) specs ...)
       (apply append
              ;; TODO: this filters on identity because I don't
              ;; handle some requires.
              ;; For example, require-spec->provides returns #f when
              ;; it can't find a .zo file
              (filter identity (map require-spec->provides specs)))]
      [else ;(warn 'expansion-identifiers
            ;      (format "Could not extract anything from program expansion: ~v"
            ;              program-sxp))
            ;(printf "couldn't find anything in: ~v~n" program-sxp)
            null]))
  
  
  (define (tab-completion-definitions-text-mixin dt%)
    (class (expansion-tool-definitions-text-mixin dt%)
      
      (super-new)
      
      ;; inherited from expansion-tool-definitions-text-mixin
      (inherit buffer-directory)
      (inherit-field latest-expansion)
      
      ;; inherited from DrScheme's definitions text
      (inherit get-start-position get-text insert position-location get-admin)
      
      (rename-super [super-on-char on-char])
      (define/override (on-char event)
        (if (and (eq? 'release (send event get-key-code))
                 (send event get-shift-down)
                 (let ([code (send event get-key-release-code)])
                   (and (char? code)
                        (char=? #\tab code))))
            (let ([menu (new popup-menu%
                             [popdown-callback (lambda (menu event)
                                                 (when-debugging
                                                  (printf "popdown callback~n"))
                                                 (case (send event get-event-type)
                                                   [(menu-popdown) 'hmm]
                                                   [(menu-popdown-none) (void)]))]
                             [demand-callback (lambda (menu)
                                                (when-debugging
                                                 (printf "demand callback~n")))])])
              (define choices (parameterize ([current-directory (buffer-directory)])
                                (program-toplevel-symbols latest-expansion)))
              (let* ([this-pos (get-start-position)]
                     [char (if (zero? this-pos) #\space (send this get-character (sub1 this-pos)))]
                     [prefix? (and (not (char-whitespace? char))
                                   (not (char-blank? char))
                                   (not (eq? #\( char))
                                   (not (eq? #\) char)))])
                (define prefix (and prefix?
                                    (let ([bsxp (send this get-backward-sexp this-pos)])
                                      (and bsxp
                                           (get-text bsxp this-pos)))))
                (define choice-strings (map symbol->string choices))
                (define prefix-choices (if prefix
                                           (let ([rx (regexp (string-append "^" prefix))])
                                             (filter (lambda (choice)
                                                       (regexp-match rx choice))
                                                     choice-strings))
                                           choice-strings))
                (for-each (lambda (choice)
                            (new menu-item% [label choice] [parent menu]
                                 [callback (lambda (item event)
                                             (define label (send item get-label))
                                             (when-debugging
                                              (printf "selected item: ~a~n" (send item get-label)))
                                             (insert (if prefix
                                                         (substring label (string-length prefix))
                                                         label)
                                                     this-pos))]))
                          prefix-choices))
              (let ([location-x-box (box 0)]
                    [location-y-box (box 0)])
                (position-location (get-start-position) location-x-box location-y-box #f)
                (send (get-admin) popup-menu
                        menu (unbox location-x-box) (unbox location-y-box))
                  (when-debugging
                   (printf "popped up the menu!~n"))))
            (super-on-char event)))
      
      ))
  
  
  )
