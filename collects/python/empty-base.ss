(module empty-base mzscheme
  (provide #%module-begin
           #%app
           #%top
           #%datum
           (rename begin #%begin)
           (rename provide #%provide)
           (rename define #%define)
           (rename require #%require)
           (rename datum->syntax-object #%datum->syntax-object)
           syntax))