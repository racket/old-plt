
(module start mzscheme
  (require "private/link.ss"
	   "private/drsig.ss"
	   (lib "class.ss")
	   (lib "unitsig.ss")
           (lib "framework.ss" "framework")
	   (lib "splash.ss" "framework"))

  (shutdown-splash)

  (define-values/invoke-unit/sig drscheme^ drscheme@)

  ;; define these two functions at the top-level
  ;; for use in the bug report form in Help Desk

  ;; get-teachpack-filenames : -> writable
  (define (get-teachpack-filenames)
    (drscheme:teachpack:teachpack-cache-filenames (preferences:get 'drscheme:teachpacks)))

  ;; get-language-level : -> writable
  (define (get-language-level)
    (let* ([language/settings (preferences:get drscheme:language-configuration:settings-preferences-symbol)]
	   [language (drscheme:language-configuration:language-settings-language language/settings)]
	   [settings (drscheme:language-configuration:language-settings-settings language/settings)])
      (list
       (send language get-language-position)
       (send language marshall-settings settings))))
  (eval `(define get-teachpack-filenames ,get-teachpack-filenames))
  (eval `(define get-language-level ,get-language-level))


  (close-splash))

