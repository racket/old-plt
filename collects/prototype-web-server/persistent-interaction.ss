(module persistent-interaction mzscheme
  (require "persistent-expander.ss")
  (provide (all-from-except mzscheme #%module-begin)
           (rename lang-module-begin #%module-begin)
           start-interaction
           send/suspend)
  )