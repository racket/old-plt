(module mzrestricted mzscheme
  (provide (all-from-except mzscheme 
                            build-path absolute-path? relative-path? complete-path?
                            path->complete-path resolve-path expand-path simplify-path
                            normal-case-path split-path find-executable-path
                            find-system-path path-list-string->path-list
                            file-exists? link-exists? delete-file rename-file-or-directory
                            file-or-directory-modify-seconds file-or-directory-permissions
                            file-size copy-file current-directory current-drive 
                            directory-exists? make-directory delete-directory
                            rename-file-or-directory file-or-directory-modify-seconds
                            directory-list filesystem-root-list)))
        