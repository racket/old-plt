(module test-helpers mzscheme
  (provide require/expose)
  
  ;; Requires a module and exposes some of its unprovided (non-syntax!)
  ;; identifiers.
  ;; USAGE: (require/expose MODULE-NAME (IDS ...))
  ;;   where MODULE-NAME is as in the MzScheme manual (i.e., a standard
  ;;   module spec) and IDS are the un-provided identifiers that you wish to
  ;;   expose in the current module.
  ;; Based on a macro by Raymond Racine (rracine@adelphia.net) posted to
  ;; plt-scheme in Dec 2003.
  (define-syntax require/expose
    (syntax-rules ()
      [(_ mod (ids ...))
       (begin
         (require mod)
         (define-values (ids ...)
           (let ([ns (module->namespace 'mod)])
             (parameterize ([current-namespace ns])
               (values
                (namespace-variable-value 'ids)...)))))]))
  )