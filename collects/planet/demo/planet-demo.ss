(module planet-demo (lib "run.ss" "slideshow")

  (require (lib "code.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "face.ss" "texpict")
	   (lib "etc.ss"))
  
  (require (prefix server: (lib "server-config.ss" "planet"))
           (lib "planet-server.ss" "planet"))

  (define server-custodian #f)
  
  (define (initialize-server)
    (parameterize ([server:PLANET-SERVER-REPOSITORY 
                    (build-path (this-expression-source-directory) "repository-1")]
                   [current-custodian (make-custodian)])
      (thread start)
      (set! server-custodian (current-custodian))))

  (define (upgrade-addblaster)
    (custodian-shutdown-all server-custodian)
    (parameterize ([server:PLANET-SERVER-REPOSITORY
                    (build-path (this-expression-source-directory) "repository-2")]
                   [current-custodian (make-custodian)])
      (thread start)
      (set! server-custodian (current-custodian))))
  
  (define (click-here-button action)
    (let ((text (frame (cc-superimpose (tt "Click here") (ghost (tt "thanks!"))))))
      (clickback text 
                 (lambda () 
                   (action)
                   (set! text (frame (cc-superimpose (ghost (tt "Click here")) (tt "thanks!"))))))))
  
  (define mike (face 'happy "yellow"))
  (define jacob (face 'mad "SteelBlue"))
  
  (define total-w (+ client-w (* 2 margin)))
  (define total-h (+ client-h (* 2 margin)))
  (define (make-assembler bkg corner)
   (lambda (s v-sep c)
     (lt-superimpose
      (lb-superimpose 
       (inset (overlay-background bkg (blank total-w total-h)) (- margin))
       corner)
      (if s
          (vc-append v-sep 
                     ;; left-aligns the title:
                     (titlet s)
                     c)
          c))))
  
  (define (overlay-background color pict)
    (let* ([w (pict-width pict)]
           [h (pict-height pict)]
           [bkg
            (dc
             (lambda (dc x y)
               (let ([old-pen (send dc get-pen)]
                     [old-brush (send dc get-brush)]
                     [pen (send the-pen-list find-or-create-pen color 1 'solid)]
                     [brush (send the-brush-list find-or-create-brush color 'solid)])
                 (unless pen
                   (error "unknown color: ~s" color))
                 (send dc set-pen pen)
                 (send dc set-brush brush)
                 (send dc draw-rectangle x y w h)
                 (send dc set-pen old-pen)
                 (send dc set-brush old-brush)))
             w
             h
             0
             0)])
      (cc-superimpose bkg pict)))
    
  (define user-assembler (make-assembler "LightYellow" (scale mike 1/5)))
  (define devel-assembler (make-assembler "AliceBlue" (scale jacob 1/5)))
  (define blank-assembler (make-assembler "White" (blank)))
  
  (define (user) (current-slide-assembler user-assembler))
  (define (developer) (current-slide-assembler devel-assembler))
  (define (blank-background) (current-slide-assembler blank-assembler))
  
  (slide/center
   (size-in-pixels (bitmap "planet-plt.png"))
   (blank)
   (titlet "An Automated Module Repository For Scheme"))
  
  (slide/center
   (tt "In this demo, you will play two roles:"))
  
  (user)
  (slide/center
   mike
   (blank)
   (hbl-append (tt "Mike, ") (t "the PLaneT user"))
   (blank)
   (t "When you are Mike, the background will look like this"))
  
  (developer)
  (slide/center
   jacob
   (blank)
   (hbl-append (tt "Jacob, ") (t "the package developer"))
   (blank)
   (t "When you are Jacob, the background will look like this"))
  
  (blank-background)
  (slide/title/center
   "Before we begin ..."
   (page-item (click-here-button initialize-server)
              " to start a fake PLaneT server that we can use for the demo")
   (page-item "Start a new DrScheme with language level set to Graphical Full Scheme"))

  (developer)
  (slide/title/center
   "AddBlaster!"
   
   (page-para "You develop the world's greatest library:")
   (blank)
   (code (module add-blaster mzscheme
           (provide add-blaster)
           (define (add-blaster n) (+ n 1))
           
           (printf "I'm Add-Blaster, version 1.0")))
   (blank)
   (page-para "and you want all of humanity to benefit from your achievement, "
              "so you decide to release it via PLaneT.")
   )
   
  (slide/title/center
   "AddBlaster!"
   (page-para "To do that, use the function " (code make-planet-archive) 
              " in " (code (lib "util.ss" "planet")) ":")
   (blank)
   (frame 
    (inset
     (vl-append
      (code > (make-planet-archive "/home/jacob/add-blaster/"))
      (code "/home/jacob/add-blaster/add-blaster.plt"))
     gap-size))
   (blank)
   (page-para "where add-blaster/ is the root directory of your library.")
   (page-para "Mail the .plt file created to me at jacobm+planet@cs.uchicago.edu "
              "telling what package this is and if it's an upgrade of a prior package"
              "or not"
              "(this is just a regular e-mail; I don't have any automation set up yet)"))
              
  (user)
  (slide/center
   (page-para "Now you're a regular PLaneT user. You've heard of a great new library and want to use it right away.")
   (page-para "Switch over to DrScheme. Type this into the definitions window: ")
   (code (require (lib "config.ss" "planet"))
         (planet-server-name "localhost")
         (require (planet 
                   "add-blaster.ss" 
                   ("jacob" "add-blaster.plt" 1 0))))
   (page-para "Now click execute.")
   (colorize (page-para/c "(As you might guess, the first two lines are just there to divert PLaneT to your local machine.)") "blue"))
  
  (slide/center
   (page-para "You should get something like:")
   (blank)
   (size-in-pixels (bitmap "talk-basic-require.png")))
   
  
  (define id (lambda (x) x))
  (let* ([filename (code "add-blaster.ss")]
         [package-path (code "jacob")]
         [package-name (code "add-blaster.plt")]
         [major-version (code 1)]
         [minor-version (code 0)]
         [code-anim
          (lambda (f p n maj min)
            (code (require (planet 
                            #,(f filename) 
                            (#,(p package-path) #,(n package-name) #,(maj major-version) #,(min minor-version))))))]
         [slide
          (lambda (f p n maj min text)
            (slide/title
             "Anatomy of a PLaneT Require"
             (blank)
             (blank)
             (page-para "The require form looks like this:")
             (code-anim f p n maj min)
             (blank)
             text))])
    (slide id id id id id (blank))
    (slide frame id id id id (t "The file to require"))
    (slide id frame id id id (t "The logical path to the archive in the repository"))
    (slide id id frame id id (t "The package name"))
    (slide id id id frame id (vc-append
                              (t "The package's major version")
                              (colorize (t "(this is exact: only major version 1 meets this requirement)") 
                                        "blue")))
    (slide id id id id frame (vc-append 
                              (t "The lowest acceptable minor version")
                              (colorize (t "(this is a minimum: higher minor versions meet this too)")
                                        "blue")))
    (slide id id id id id (page-para "When evaluated, the PLaneT client tries to satisfy this spec with a package it already has locally"))
    (slide id id id id id (page-para "If it can't, it asks the server for the best available package that meets the specification"))
    )
   
  
  (developer)
  (slide/title/center
   "Upgrading"
   
   (page-para "Now you're the developer again. In a 24-hour hack-fest,"
              "you write an update to your wildly successful AddBlaster! "
              "library that maintains the same interface:")
   (blank)
   (code (module add-blaster mzscheme
           (provide add-blaster)
           (define add-blaster add1)
           
           (printf "I'm Add-Blaster, version 1.1")))
   (blank))
  
  (slide/title/center
   "Upgrading"
   
   (page-para "Again, run " (code make-planet-archive) " on the directory "
              "that contains your updated library and mail it to me at"
              "jacobm+planet@cs.uchicago.edu. Indicate whether or not you're sending "
              "a backwards-compatible update.")
   (blank)
   (page-para (click-here-button upgrade-addblaster) " to make an upgrade to AddBlaster available for the purposes of the demo"))
  
  
  (user)
  (slide/title/center
   "Upgrading"
   (page-para "Now back in DrScheme, click execute again, with ")
   (code (require (lib "config.ss" "planet"))
         (planet-server-name "localhost")
         (require (planet 
                   "add-blaster.ss" 
                   ("jacob" "add-blaster.plt" 1 0))))
   (page-para "still in the definitions window."))
  
  (slide/title/center
   "Upgrading"
   (size-in-pixels (bitmap "talk-basic-require.png"))
   (blank)
   (page-para "Since your local PLaneT cache has an acceptable package, "
              "it doesn't check the server for possible upgrades."))
   
  (slide/title/center
   "Upgrading"
   (page-para "To see the upgrade, require version 1.1 explicitly:")
   (blank)
   (code (require (planet
                   "add-blaster.ss"
                   ("jacob" "add-blaster.plt" 1 1))))
   (page-para "and click execute again")
   'next
   (size-in-pixels (bitmap "talk-upgrade-require.png")))
  
  (slide/title/center
   "Upgrading"
   (page-para "Now the local cache has version 1.1, so")
   (code (require (planet
                   "add-blaster.ss"
                   ("jacob" "add-blaster.plt" 1 0))))
   (page-para "fetches library version 1.1:")
   (blank)
   (size-in-pixels (bitmap "talk-upgrade-require-2.png")))
  
  (slide/title/center
   "Upgrading"
   (page-para "However, this behavior is only evident at the REPL. If you require a "
              "PLaneT library from within a module saved in a file, it is \"linked\" "
              "to the version it initially receives. It will always get that exact "
              "version in the future, even if an upgrade becomes available"))
 
  (let* ([mod (lambda (name body) 
                (cc-superimpose (lt-superimpose 
                                 (frame 
                                  (blank 
                                   (max (+ (pict-width body) 40) (+ (pict-width (t name)) 40))
                                   (+ (pict-height body) (pict-height (t name)) 40)))
                                 (t name))
                                body))]
        
         [a (mod "a.ss" (code (require a b)))]
         [b (mod "b.ss" (code (require (planet 
                                        d
                                        1.0))))]
         [c (mod "c.ss" (code (require (planet
                                        d
                                        1.1))))]
         [d (mod "d.ss" (code ...))]
         [predag
          (vc-append
           40
           a
           (hc-append 40 b c)
           d)]
         [arrow (lambda (pict src dest) (add-arrow-line 15 pict src find-cb dest find-ct))]
         [dag (arrow (arrow (arrow (arrow predag a b) a c) b d) c d)])

    (slide/title/center
     "Conflicts"
     (page-para "The situation can arise that two different modules within the same "
                "program want two different versions of the same library")
     (blank)
     dag))
  
  (slide/title/center
   "Conflicts"
   (page-item (t "This could be a problem if libraries keep internal state"))
   (page-item (t "The PLaneT client detects this situation and raises an error"))
   (blank)
   (size-in-pixels (bitmap "talk-conflict.png")))
  
  (blank-background)
  
  (slide/center
   (page-para "That's the basics of PLaneT. The first alpha release of PLaneT will be available "
              "by Monday; once the PLaneT server is accessible until I get an discovery system "
              "in place I'll manually publish a list of available packages.")
   (page-para "I very much want feedback on the system; once you've played around with it a "
              "little please send me your thoughts. And, of course, your bug reports!")
   (blank)
   (titlet "The End")))
  
  
                            
