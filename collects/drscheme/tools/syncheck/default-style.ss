(module default-style mzscheme
  (provide keyword-color
           keyword-style
           unbound-variable-color
           unbound-variable-style
           bound-variable-color
           bound-variable-style
           primitive-color
           primitive-style
           constant-color
           constant-style
           comment-color
           comment-style
           base-color
           base-style)
  
  (define bw? (< (mred:get-display-depth) 8))
  
  (define keyword-color '(40 25 15))
  (define keyword-style 'bold)
  (define unbound-variable-color (if bw? "black" "red"))
  (define unbound-style (if bw? 'slant 'default)) 
  (define bound-variable-color (if bw? "black" "navy"))
  (define bound-variable-style (if bw? 'underline 'default))
  (define primitive-color "navy")
  (define primitive-style 'default)
  (define constant-color '(51 135 39)) ;; green
  (define constant-style 'default)
  (define comment-color "teal")
  (define comment-style "teal")
  (define base-color "brown")
  (define base-style 'normal)))
