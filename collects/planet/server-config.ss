(module server-config mzscheme
  
  (require "private/planet-shared.ss"
           (lib "etc.ss"))
  
  (define-parameters 
   (PLANET-SERVER-REPOSITORY (build-path (this-expression-source-directory) "repository"))
   (PLANET-SERVER-PORT       270)
   
   (METAINFO-FILE            "planet.txt")
   
   (PLANET-LOG-DIR           (this-expression-source-directory))
   (PLANET-ERROR-LOG         (build-path (PLANET-LOG-DIR) "ERRORS"))
   (PLANET-CONNECT-LOG       (build-path (PLANET-LOG-DIR) "LOG"))))
