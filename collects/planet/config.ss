(module config mzscheme
  
  (require "private/planet-shared.ss"
           (lib "etc.ss"))
  
  (define-parameters (PLANET-SERVER-NAME       "planet.plt-scheme.org")
                     (PLANET-SERVER-PORT       270)
                     (PLANET-DIR               (build-path (find-system-path 'addon-dir) "planet"))
                     (CACHE-DIR                (build-path (PLANET-DIR) "cache"))
                     (LINKAGE-FILE             (build-path (PLANET-DIR) "LINKAGE"))                  
                     (LOGGING-ENABLED?         #t)
                     (LOG-FILE                 (build-path (PLANET-DIR) "INSTALL-LOG"))
                     (DEFAULT-PACKAGE-LANGUAGE (version))))
