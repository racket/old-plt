(module acks mzscheme
  (provide general-acks translating-acks authors)
  
  (define authors
    (string-append
     "DrScheme was written by "
     "John Clements, "
     "Matthias Felleisen, "
     "Robby Findler, "
     "Paul Graunke, "
     "Matthew Flatt, "
     "Shriram Krishnamurthi, "
     "and "
     "Paul Steckler."))
  
  (define general-acks
    (string-append
     "Thanks to"
     "Ian Barland, "
     "Eli Barzilay, "
     "Gann Bierner, "
     "Richard Cobbe, "
     "Moy Easwaran, "
     "Kathy Fisler, "
     "Cormac Flanagan, "
     "Sebastian Good, "
     "Kathy Gray, "
     "Mark Krentel, "
     "Mario Latendresse, "
     "Scott Owens, "
     "Jamie Raymond, "
     "Dorai Sitaram, "
     "Mike Sperber, "
     "Francisco Solsona, "
     "Stephanie Weirich, "
     "Noel Welsh, "
     "and "
     "Adam Wick "
     "for contributions of prototypes, libraries, and criticism of PLT documentation."))

  (define translating-acks
    (string-append
     "Thanks to "
     "Ian Barland, "
     "Tim Hanson, "
     "Philippe Meunier, "
     "Francisco Solsona, "
     "and "
     "Reini Urban "
     "for their help translating DrScheme's GUI to other languages.")))
  


