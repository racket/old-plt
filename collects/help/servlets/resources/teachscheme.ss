(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (HEAD ,hd-css
         (TITLE "TeachScheme! Workshops"))
   (BODY 
    (H1  "TeachScheme! Workshops")	
    (A ((NAME "workshops") (VALUE "TeachScheme! workshops")))
    "TeachScheme! is a free summer workshop for high school teachers. "
    "Its goal is to bridge the gulf between high school and "
    "college-level computing curricula.  In the workshop, programming "
    "is taught as an algebraic problem-solving process, and computing "
    "is the natural generalization of grade-school level calculating." 
    (P)
    "Students who learn to design programs properly learn to "
    "analyze a problem statement; express its essence, abstractly "
    "and with examples; formulate statements and comments in a "
    "precise language; evaluate and revise these activities in "
    "light of checks and tests; and pay attention to details. "
    "As a result, all students benefit, those who wish to study computing "
    "as well as those who just wish to explore the subject."
    (P)
    "For more information, see the " 
    (A ((HREF "http://www.teach-scheme.org/Workshops/"))
       "TeachScheme! Workshops page") ".")))
