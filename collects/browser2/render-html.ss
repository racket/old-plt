(require-library "macro.ss")
(require-library "function.ss")
(require-library "xml.ss" "xml")
(require-library "html.ss" "html")
(require-library "url.ss" "net")

(load "render-table-2.ss")
(load "utils.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TEST CASES inside render-html-test.ss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA DEFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; > anchor 
;  (make-anchor name location)
;  name : string
;  location : integer
(define-struct anchor (name location))

; An hr-snip% is an editor-snip% used to represent the 'horizontal rule';
; it has a SIZE (height), WIDTH, and COLOR, as well as NOSHADE (no fill).
; - size and width are integers
; - color is color% object
; - noshade is boolean
(define hr-snip%
  (class editor-snip% (size width color noshade)
    (rename [super-draw draw])
    (inherit get-admin)
    (sequence (super-init #f #t 0 0 0 0 0 0 0 0 width width size size))
    (override 
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
         (super-draw dc x y left top right bottom dx dy draw-caret)
         (local [(define orig-pen (send dc get-pen))
                 (define orig-brush (send dc get-brush))
                 (define admin (get-admin))
                 (define x1 (- x dx))
                 (define y1 (- y dy))
                 (define x2 (box x))
                 (define y2 (box y))
                 (define (width) (+ 5 (- (unbox x2) x1)))
                 (define (height) (+ 5 (- (unbox y2) y1)))]
           (unless (boolean? admin)
             (send (send admin get-editor) get-snip-location this x2 y2 #t)
             (send dc set-pen (send the-pen-list find-or-create-pen color 1 'solid))
             (if (not noshade)
                 (send dc set-brush (send the-brush-list find-or-create-brush color 'solid)))
             (send dc draw-rectangle x1 y1 (width) (height))
             (send dc set-pen orig-pen)
             (send dc set-brush orig-brush))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GLOBAL VARIABLE DEFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fontsizesvector (vector 8 12 14 18 22 26 32))
(define HeadingSizesVector (vector 48 42 36 28 22 18))
(define basefontsize 1) ;index=0
(define base-style-delta (make-object style-delta%))
(define local-anchors empty) ; listof anchors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; paragraphicize : text% p-struct -> void
; To render the paragraph on a-text, with any number of attributes,
; or none.
; **supports only the "align" attribute, with values "center",
;   "right", and "left"
; **does not support the attribute value "justify" (treats it as "left")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (paragraphicize a-text a-p)
  (local [(define attribs (html:html-element-attributes a-p))
          (define G5 (first (html:html-full-content a-p)))
          (define alignment (get-attribute-value attribs 'align "left"))
          (define startline (send a-text last-line))
          (define startlinelen (send a-text line-length startline))
          (define (prevlinelen) (send a-text line-length (sub1 startline)))
          (define (thunk)
            (align-paragraph a-text G5 (lambda (G5) (render-G5 a-text G5)) alignment)
            (when (or (not (zero? (send a-text line-length (send a-text last-line))))
                      (not (zero? (send a-text line-length (sub1 (send a-text last-line))))))
              (send a-text insert #\newline)))]
    (cond [(not (zero? startlinelen))
           (send a-text insert #\newline)
           (send a-text insert #\newline)
           (thunk)]
          [(or (zero? startline)
               (= 1 (prevlinelen)))
           (thunk)]
          [else
           (send a-text insert #\newline)
           (thunk)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; boldify : text% num num -> void
; Turns the region from x to y bold in the text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (boldify a-text x y)
  (local [(define bold-style-delta (make-object style-delta% 'change-bold))]
    (send a-text change-style bold-style-delta x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; italicize : text% num num -> void
; Italicizes the region from x to y in the text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (italicize a-text x y)
  (local [(define italic-style-delta (make-object style-delta% 'change-italic))]
    (send a-text change-style italic-style-delta x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; underline : text% num num -> void
; Underlines the region from x to y in the text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (underline a-text x y)
  (local [(define u-style-delta (make-object style-delta% 'change-underline #t))]
    (send a-text change-style u-style-delta x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fontify : text% Attribute num num -> void
; Changes font attribute in a-text between x and y.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fontify a-text attrib x y)
  (cond [(symbol=? 'size (attribute-name attrib)) 
         (changefontsize a-text (attribute-value attrib) x y)]
        [(symbol=? 'color (attribute-name attrib))
         (changefontcolor a-text (attribute-value attrib) x y)]
        [(symbol=? 'face (attribute-name attrib))
         (changefontface a-text (attribute-value attrib) x y)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; base-fontify! : Attribute -> void
; Changes basefont attribute by changing the state of
; base-style-delta and pointers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (base-fontify! attrib)
  (cond [(symbol=? 'size (attribute-name attrib)) 
         (change-base-font-size! (attribute-value attrib))]
        [(symbol=? 'color (attribute-name attrib))
         (change-base-font-color! (attribute-value attrib))]
        [(symbol=? 'face (attribute-name attrib))
         (change-base-font-face! (attribute-value attrib))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; changefontface : text string num num -> void
; Searches list of recognizable font faces and families as listed
; in a-string.  Changes the font face to the matched substring
; on a-text from x to y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (changefontface a-text a-string x y)
  (local [(define system-faces (get-face-list))
          (define system-families (map symbol->string '(default decorative roman script swiss modern symbol system)))
          (define cdata-names (comma-delimited-text-parser a-string))
          (define (findface alos)
            (cond [(empty? alos) #f]
                  [(boolean? (member (first alos) system-faces)) (findface (rest alos))]
                  [else (first (member (first alos) system-faces))]))
          (define (findfamily alos)
            (cond [(empty? alos) #f]
                  [(boolean? (member (first alos) system-families)) (findfamily (rest alos))]
                  [else (first (member (first alos) system-families))]))
          (define fontstyledelta (make-object style-delta%))
          (define face (findface cdata-names))
          (define family (findfamily cdata-names))]
    (cond [(not (boolean? face))
           (send a-text change-style 
                 (send fontstyledelta set-delta-face face 'base) 
                 x y)]
          [(not (boolean? family))
           (send fontstyledelta set-family (string->symbol family))
           (send a-text change-style fontstyledelta x y)]
          [else (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; changefontcolor : text string num num -> void
; Changes the foreground color on a-text between x and y to
; the color specified in a-string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (changefontcolor a-text a-string x y)
  (cond [(char=? #\# (string-ref a-string 0))
         (change-font-color-RGB a-text a-string x y)]
        [else (change-font-color-name a-text a-string x y)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-font-color-name : text string num num -> void
; Changes the foreground color on a-text between x and y to the
; color specified in a-string.
; The color must be one of
; - black  
; - silver
; - gray  
; - white  
; - maroon 
; - red  
; - purple 
; - fuchsia
; - green  
; - lime  
; - olive 
; - yellow
; - navy
; - blue
; - teal
; - aqua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-font-color-name a-text a-string x y)
  (local [(define RRGGBB 
            (cond [(string-ci=? a-string "black")
                   "#000000"]
                  [(string-ci=? a-string "silver")
                   "#C0C0C0"]
                  [(string-ci=? a-string "gray")
                   "#808080"]
                  [(string-ci=? a-string "white")
                   "#FFFFFF"]
                  [(string-ci=? a-string "maroon")
                   "#800000"]
                  [(string-ci=? a-string "red")
                   "#FF0000"]
                  [(string-ci=? a-string "purple")
                   "#800080"]
                  [(string-ci=? a-string "fuchsia")
                   "#FF00FF"]
                  [(string-ci=? a-string "green")
                   "#008000"]
                  [(string-ci=? a-string "lime")
                   "#00FF00"]
                  [(string-ci=? a-string "olive")
                   "#808000"]
                  [(string-ci=? a-string "yellow")
                   "#FFFF00"]
                  [(string-ci=? a-string "navy")
                   "#000080"]
                  [(string-ci=? a-string "blue")
                   "#0000FF"]
                  [(string-ci=? a-string "teal")
                   "#008080"]
                  [(string-ci=? a-string "aqua")
                   "#00FFFF"]
                  [else (error 'change-font-color-RGB "Unknown color: ~s" a-string)]))]
    (change-font-color-RGB a-text RRGGBB x y)))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-font-color-RGB : a-text string num num -> void
; Changes the foreground color on a-text between x and y to the
; hexadecimal RGB value specified in a-string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-font-color-RGB a-text a-string x y)
  (local [(define RRGGBB (list->string (rest (string->list a-string))))
          (define a-color (make-object color% 
                            (hex->integer (substring RRGGBB 0 2))
                            (hex->integer (substring RRGGBB 2 4))
                            (hex->integer (substring RRGGBB 4 6))))
          (define color-style-delta (send 
                                     (make-object style-delta%)
                                     set-delta-foreground
                                     a-color))]
    (send a-text change-style color-style-delta x y)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; changefontsize : a-text a-string num num -> void
; Increments/decrements font size by integer amount in a-string
; on a-text from x to y.  If first character in a-string is + or
; -, then increment/decrement is relative to basefontsize.
; Otherwise, a-string is assumed to be an integer representing
; the fixed size of the font.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (changefontsize a-text a-string x y)
  (local [(define (relative? a-string)
            (cond [(or (char=? (string-ref a-string 0) #\+)
                       (char=? (string-ref a-string 0) #\-))
                   true]
                  [else false]))]
    (cond [(relative? a-string) (change-font-relative-size a-text (string->number a-string) x y)]
          [else (change-font-fixed-size a-text (string->number a-string) x y)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-font-relative-size : a-text num num num -> void
; Increments/decrements font size by integer amount of a-num
; relative to basefont size on a-text from x to y. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-font-relative-size a-text a-num x y)
  (local [(define newfontsize (+ basefontsize a-num))
          (define (fontsizestyledelta)
            (cond [(and (<= newfontsize (vector-length fontsizesvector))
                        (>= newfontsize 1))
                   (make-object style-delta% 'change-size 
                     (vector-ref
                      fontsizesvector 
                      (sub1 newfontsize)))]
                  [(> newfontsize (vector-length fontsizesvector))
                   (make-object style-delta% 'change-size 
                     (vector-ref
                      fontsizesvector
                      (sub1 (vector-length fontsizesvector))))]
                  [(< newfontsize 1)
                   (make-object style-delta% 'change-size
                     (vector-ref
                      fontsizesvector
                      0))]))]
    (send a-text change-style (fontsizestyledelta) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-font-fixed-size : a-text num num num -> void
; Changes font size to a-num on a-text from x to y.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-font-fixed-size a-text a-num x y)
  (local [(define (fontsizestyledelta)
            (cond [(and (<= a-num (vector-length fontsizesvector))
                        (>= a-num 1))
                   (make-object style-delta% 'change-size 
                     (vector-ref
                      fontsizesvector 
                      (sub1 a-num)))]
                  [(> a-num (vector-length fontsizesvector))
                   (make-object style-delta% 'change-size 
                     (vector-ref
                      fontsizesvector
                      (sub1 (vector-length fontsizesvector))))]
                  [(< a-num 1)
                   (make-object style-delta% 'change-size
                     (vector-ref
                      fontsizesvector
                      0))]))]
    (send a-text change-style (fontsizestyledelta) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-base-font-size! : string -> void
; Permanently changes font size in base-style-delta to integer 
; value of a-string.
; **fixed sizing only, no relative sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-base-font-size! a-string)
  (local [(define new-font-spec (string->number a-string))
          (define new-font-size
            (cond [(and (<= new-font-spec (vector-length fontsizesvector))
                        (>= new-font-spec 1))
                   new-font-spec]
                  [(> new-font-spec (vector-length fontsizesvector))
                   (vector-length fontsizesvector)]
                  [(< new-font-spec 1)
                   1]))]
    (set! base-style-delta (send base-style-delta set-delta 'change-size 
                                 (vector-ref fontsizesvector (sub1 new-font-size))))
    (set! basefontsize new-font-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-base-font-color! : string -> void
; Permanently changes font color in base-style-delta to color
; specified in a-string.  A-string can be one of 16 recognized
; color names or the hexadecimal RGB representation in the form
; of "#RRGGBB".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-base-font-color! a-string)
  (cond [(char=? #\# (string-ref a-string 0))
         (change-base-font-color-RGB! a-string)]
        [else (change-base-font-color-name! a-string)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-base-font-color-RGB! : string -> void
; Changes the foreground color in base-style-delta to the 
; hexadecimal RGB value specified in a-string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-base-font-color-RGB! a-string)
  (local [(define RRGGBB (list->string (rest (string->list a-string))))
          (define a-color (make-object color% 
                            (hex->integer (substring RRGGBB 0 2))
                            (hex->integer (substring RRGGBB 2 4))
                            (hex->integer (substring RRGGBB 4 6))))]
    (send base-style-delta set-delta-foreground a-color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-base-font-color-name! : string -> void
; Changes the foreground color in base-style-delta to the 
; recognizable color in a-string, one of 16 possible colors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-base-font-color-name! a-string)
    (local [(define RRGGBB 
              (cond [(string-ci=? a-string "black")
                     "#000000"]
                    [(string-ci=? a-string "silver")
                     "#C0C0C0"]
                    [(string-ci=? a-string "gray")
                     "#808080"]
                    [(string-ci=? a-string "white")
                     "#FFFFFF"]
                    [(string-ci=? a-string "maroon")
                     "#800000"]
                    [(string-ci=? a-string "red")
                     "#FF0000"]
                    [(string-ci=? a-string "purple")
                     "#800080"]
                    [(string-ci=? a-string "fuchsia")
                     "#FF00FF"]
                    [(string-ci=? a-string "green")
                     "#008000"]
                    [(string-ci=? a-string "lime")
                     "#00FF00"]
                    [(string-ci=? a-string "olive")
                     "#808000"]
                    [(string-ci=? a-string "yellow")
                     "#FFFF00"]
                    [(string-ci=? a-string "navy")
                     "#000080"]
                    [(string-ci=? a-string "blue")
                     "#0000FF"]
                    [(string-ci=? a-string "teal")
                     "#008080"]
                    [(string-ci=? a-string "aqua")
                     "#00FFFF"]))]
      (change-base-font-color-RGB! RRGGBB)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change-base-font-face! : string -> void
; Searches list of recognizable font faces and families as listed
; in a-string.  Permanently changes the base-style-delta font face
; to the matched substring.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-base-font-face! a-string)
  (local [(define system-faces (get-face-list))
          (define system-families (map symbol->string '(default decorative roman script swiss modern symbol system)))
          (define cdata-names (comma-delimited-text-parser a-string))
          (define (findface alos)
            (cond [(empty? alos) #f]
                  [(boolean? (member (first alos) system-faces)) (findface (rest alos))]
                  [else (first (member (first alos) system-faces))]))
          (define (findfamily alos)
            (cond [(empty? alos) #f]
                  [(boolean? (member (first alos) system-families)) (findfamily (rest alos))]
                  [else (first (member (first alos) system-families))]))
          (define face (findface cdata-names))
          (define family (findfamily cdata-names))]
    (cond [(not (boolean? face))
           (send base-style-delta set-delta-face face 'base)]
          [(not (boolean? family))
           (send base-style-delta set-family (string->symbol family))]
          [else (void)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G2? : html-element -> boolean
; Determines if a-html-element belongs to G2, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G2? a-html-element)
  (cond [(html:form? a-html-element) true]
        [(G3? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G3? : html-element -> boolean
; Determines if a-html-element belongs to G3, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G3? a-html-element)
  (cond [(html:fieldset? a-html-element) true]
        [(html:isindex? a-html-element) true]
        [(G4? a-html-element) true]
        [(G11? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G4? : html-element -> boolean
; Determines if a-html-element belongs to G4, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G4? a-html-element)
  (cond [(G8? a-html-element) true]
        [(G10? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G5? : html-element -> boolean
; Determines if a-html-element belongs to G5, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G5? a-html-element)
  (cond [(html:label? a-html-element) true]
        [(G6? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G6? : html-element -> boolean
; Determines if a-html-element belongs to G6, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G6? a-html-element)
  (cond [(html:a? a-html-element) true]
        [(G7? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G7? : html-element -> boolean
; Determines if a-html-element belongs to G7, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G7? a-html-element)
  (cond [(G8? a-html-element) true]
        [(G12? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G8? : html-element -> boolean
; Determines if a-html-element belongs to G8, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G8? a-html-element)
  (cond [(html:applet? a-html-element) true]
        [(html:basefont? a-html-element) true]
        [(html:big? a-html-element) true]
        [(html:font? a-html-element) true]
        [(html:img? a-html-element) true]
        [(html:object? a-html-element) true]
        [(html:small? a-html-element) true]
        [(html:sub? a-html-element) true]
        [(html:sup? a-html-element) true]
        [(G9? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G9? : html-element -> boolean
; Determines if a-html-element belongs to G9, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G9? a-html-element)
  (cond [(html:abbr? a-html-element) true]
        [(html:acronym? a-html-element) true]
        [(html:b? a-html-element) true]
        [(html:bdo? a-html-element) true]
        [(html:br? a-html-element) true]
        [(html:cite? a-html-element) true]
        [(html:code? a-html-element) true]
        [(html:dfn? a-html-element) true]
        [(html:em? a-html-element) true]
        [(html:i? a-html-element) true]
        [(html:kbd? a-html-element) true]
        [(html:map? a-html-element) true]
        [(pcdata? a-html-element) true]
        [(html:q? a-html-element) true]
        [(html:s? a-html-element) true]
        [(html:samp? a-html-element) true]
        [(html:script? a-html-element) true]
        [(html:span? a-html-element) true]
        [(html:strike? a-html-element) true]
        [(html:strong? a-html-element) true]
        [(html:tt? a-html-element) true]
        [(html:u? a-html-element) true]
        [(html:var? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G10? : html-element -> boolean
; Determines if a-html-element belongs to G10, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G10? a-html-element)
  (cond [(html:address? a-html-element) true]
        [(html:blockquote? a-html-element) true]
        [(html:center? a-html-element) true]
        [(html:dir? a-html-element) true]
        [(html:div? a-html-element) true]
        [(html:dl? a-html-element) true]
        [(html:h1? a-html-element) true]
        [(html:h2? a-html-element) true]
        [(html:h3? a-html-element) true]
        [(html:h4? a-html-element) true]
        [(html:h5? a-html-element) true]
        [(html:h6? a-html-element) true]
        [(html:hr? a-html-element) true]
        [(html:menu? a-html-element) true]
        [(html:noframes? a-html-element) true]
        [(html:noscript? a-html-element) true]
        [(html:ol? a-html-element) true]
        [(html:p? a-html-element) true]
        [(html:pre? a-html-element) true]
        [(html:table? a-html-element) true]
        [(html:ul? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G11? : html-element -> boolean
; Determines if a-html-element belongs to G11, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G11? a-html-element)
  (cond [(html:a? a-html-element) true]
        [(html:label? a-html-element) true]
        [(G12? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G12? : html-element -> boolean
; Determines if a-html-element belongs to G12, returns true or false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (G12? a-html-element)
  (cond [(html:button? a-html-element) true]
        [(html:iframe? a-html-element) true]
        [(html:input? a-html-element) true]
        [(html:select? a-html-element) true]
        [(html:textarea? a-html-element) true]
        [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-html : text% html -> void
; Consumes a text object and html struct.  Renders the html-struct, 
; (which constitutes an html page) on the text object, returning void.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLES:
; (render-html (make-object text%)
;              (make-html empty empty)) -> void
; (render-html (make-object text%)
;              (make-html empty (make-body empty (list (make-pcdata 1 1 "PCDATA"))))) -> void
; (render-html (make-object text%)
;              (make-html empty (make-body empty (list (make-b empty (make-pcdata 1 1 "PCDATA")))))) -> void
; TEST CASES:  See "render-html-test.ss"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-html a-text a-html)
  (local [(define aloContents (filter (lambda (x)
                                        (or (html:body? x)
                                            (html:head? x)))
                                      (html:html-full-content a-html)))]
    (send a-text begin-edit-sequence)
    (for-each (lambda (contents-of-html) 
                (render-Contents-of-html a-text contents-of-html))
              aloContents)
    (send a-text end-edit-sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-Contents-of-html : text% Contents-of-html -> void
; Determines whether Contents is a body-struct or head-struct and 
; calls the appropriate function to render it.
; EXAMPLES:
; (render-Contents-of-html (make-object text%)
;                          (make-body empty empty)) -> void
; (render-Contents-of-html (make-object text%)
;                          (make-head empty empty)) -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-Contents-of-html a-text Contents)
  (cond [(html:body? Contents) (render-body a-text Contents)]
        [(html:head? Contents) (render-head a-text Contents)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-body : text% body -> void
; Renders the a-body on a-text, returning nothing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLES:
; (render-body (make-object text%)
;              (make-body empty (list (make-pcdata 1 1 "Pcdata")))) -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-body a-text a-body)
  (local [(define aloContents (html:html-full-content a-body))]
    (for-each 
     (lambda (bodycontents) (render-Contents-of-body a-text bodycontents))
     aloContents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-Contents-of-body : text% Contents-of-body -> void
; Determines whether Contents is a Del, Ins, or G2, and calls
; the appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-Contents-of-body a-text Contents)
  (cond [(html:del? Contents) (render-del a-text Contents)]
        [(html:ins? Contents) (render-ins a-text Contents)]
        [(G2? Contents) (render-G2 a-text Contents)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-head : text% head -> void
; Renders the a-head on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-head a-text a-head)
  (local [(define aloheadcontents (html:html-full-content a-head))]
    (for-each (lambda (headcontents) (render-Contents-of-head a-text headcontents))
              aloheadcontents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-Contents-of-head : text% Contents-of-head -> void
; Determines how to render the Contents by determining if it is
; one of base, isindex, link, meta, object, script, style, or 
; title, and calls the appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-Contents-of-head a-text Contents)
  (cond [(html:base? Contents) (render-base Contents)]
        [(html:isindex? Contents) (render-isindex Contents)]
        [(html:link? Contents) (render-link Contents)]
        [(html:meta? Contents) (render-meta Contents)]
        [(html:object? Contents) (render-html-object Contents)]
        [(html:script? Contents) (render-script Contents)]
        [(html:style? Contents) (render-style Contents)]
        [(html:title? Contents) (render-title Contents)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-a : text% a -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-a a-text a-a)
  (local [(define attributes (html:html-element-attributes a-a))
          (define Contents-of-a (html:html-full-content a-a))
          (define initpos (send a-text get-start-position))
          (define (finalpos) (send a-text get-start-position))
          (define url (string->url (get-attribute-value attributes 'href "")))
          (define name (get-attribute-value attributes 'name ""))
          (define (open-url a-text start end)
            (local [(define scheme (url-scheme url))
                    (define fragment (url-fragment url))
                    (define target-location (get-anchor-location fragment))]
              (cond [(and (boolean? scheme)
                          (empty? target-location))
                     (void)]
                    [(and (boolean? scheme)
                          (integer? target-location))
                     (send a-text scroll-to-position target-location #f (+ (send a-text last-position) target-location) 'start)]
                    [(and (string-ci=? scheme "http")
                          (empty? target-location))
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))]
                    [(and (string-ci=? scheme "http")
                          (integer? target-location))
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))
                     (send a-text scroll-to-position target-location #f (+ (send a-text last-position) target-location) 'start)]
                    [else 
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))])))
          (define (colorize-anchor)
            (cond [(and (boolean? (url-scheme url))
                        (boolean? (url-fragment url))) (void)]
                  [else            
                   (underline a-text initpos (finalpos))
                   (changefontcolor a-text "blue" initpos (finalpos))]))]
    (cond [(empty? Contents-of-a) (void)]
          [else
           (put-anchor! name initpos)
           (for-each (lambda (a)
                       (render-Contents-of-a a-text a))
                     Contents-of-a)
           (send a-text set-clickback initpos (finalpos) open-url #f #f)
           (colorize-anchor)])))

; get-anchor-location : boolean | string -> integer | empty
; Returns the location field of the anchor with name in local-anchors.
(define (get-anchor-location name)
  (local [(define (helper list)
            (cond [(empty? list) empty]
                  [(string=? (anchor-name (first list)) name) (anchor-location (first list))]
                  [else (helper (rest list))]))]
    (cond [(boolean? name) empty]
          [else (helper local-anchors)])))

; put-anchor! : string num -> (void)
; Inserts new anchor with name and location.
; EFFECT: Adds anchor to local-anchors.
(define (put-anchor! name location)
  (local [(define name-len (string-length name))]
    (cond [(<= (string-length name) 1) (void)]
          [else (set! local-anchors (cons (make-anchor (substring name 1 name-len) location) local-anchors))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-Contents-of-a : text% Contents-of-a -> void
; Calls render-Label or render-G7.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-Contents-of-a a-text Contents)
  (cond [(html:label? Contents) (render-label Contents)]
        [(G7? Contents) (render-G7 a-text Contents)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-abbr : text% abbr -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-abbr a-text a-abbr)
  ("render-abbr not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-acronym : text% acronym -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-acronym a-text a-acronym)
  ("render-acronym not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-address : text% address -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-address a-text a-address)
  ("render-address not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-applet : text% applet -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-applet a-text a-applet)
  ("render-applet not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-area : text% area -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-area a-text a-area)
  ("render-area not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-b : text% b -> void
; Consumes a text and renders the G5 bold on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-b a-text a-b)
  (local [(define G5 (first(html:html-full-content a-b)))
          (define initpos (send a-text get-start-position))]
    (render-G5 a-text G5)
    (boldify a-text initpos (send a-text get-start-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-base : text% base -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-base a-text a-base)
  ("render-base not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-basefont : text% basefont -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-basefont a-text a-basefont)
  (local [(define attributes (html:html-element-attributes a-basefont))]
    (for-each (lambda (attrib) (base-fontify! attrib))
              attributes)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-bdo : text% bdo -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-bdo a-text a-bdo)
  ("render-bdo not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-big : text% html:big -> void
; Renders a-big relatively 2 sizes larger on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-big a-text a-big)
  (local [(define howbig 1)
          (define G5 (filter (lambda (x)
                               (G5? x))
                             (html:html-full-content a-big)))]
    (cond [(empty? G5) (void)]
          [else
           (render-G5-with-relative-size a-text (first G5) howbig)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-blockquote : text% blockquote -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-blockquote a-text a-blockquote)
  ("render-blockquote not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-br : text% br -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-br a-text a-br)
  (send a-text insert #\newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-button : text% button -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-button a-text a-button)
  ("render-button not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-caption : text% caption -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-caption a-text a-caption)
  ("render-caption not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-center : text% center -> void
; Renders an element in the middle of the browser window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-center a-text a-center)
  (local [(define a-lo-G2 (filter (lambda (x)
                                    (G2? x))
                                  (html:html-full-content a-center)))]
    (cond [(empty? a-lo-G2) (void)]
          [else 
           (map (lambda (a-G2)
                  (align-paragraph a-text a-G2 (lambda (G2) 
                                                 (render-G2 a-text G2)) "center"))
                a-lo-G2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; align-paragraph : text% G2 (G2 -> void) string -> void
; To align a-G2, rendered by render-func, on a-text.  Acceptable
; alignment values are "left", "right", "center", and "justify".
; ** Does not 'justify text between margins.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (align-paragraph a-text a-G2 render-func how-to-align)
  (local [(define startline (send a-text last-line))
          (define startparag (send a-text line-paragraph startline))
          (define startlinelen (send a-text line-length startline))
          (define (lastline) (send a-text last-line))
          (define (lastparag) (send a-text line-paragraph (lastline)))
          (define (lastlinelen) (send a-text line-length (lastline)))
          (define alignment-symbol 
            (cond
              [(or (string-ci=? how-to-align "left") (string-ci=? how-to-align "justify")) 'left]
              [(string-ci=? how-to-align "right") 'right]
              [(string-ci=? how-to-align "center") 'center]))
          (define (align x y)
            (cond [(and (= x y)
                        (not (zero? (lastlinelen))))
                   (send a-text set-paragraph-alignment x alignment-symbol)
                   (send a-text insert #\newline)]
                  [(and (= x y)
                        (zero? (lastlinelen)))
                   (send a-text set-paragraph-alignment x alignment-symbol)
                   (send a-text insert #\newline)]
                  [(> y x)
                   (send a-text set-paragraph-alignment x alignment-symbol)
                   (align (add1 x) y)]
                  [else (error 'align-paragraph "Invalid start and stop paragraphs: x=~s y=~s" 
                               (number->string x)
                               (number->string y))]))]
     (cond [(zero? startlinelen)
            (render-func a-G2)
            (align startparag (lastparag))]
           [else
            (send a-text insert #\newline)
            (render-func a-G2)
            (align (add1 startparag) (lastparag))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-cite : text% cite -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-cite a-text a-cite)
  ("render-cite not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-code : text% code -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-code a-text a-code)
  ("render-code not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-col : text% col -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-col a-text a-col)
  ("render-col not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-colgroup : text% colgroup -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-colgroup a-text a-colgroup)
  ("render-colgroup not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-del : text% del -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-del a-text a-del)
  ("render-del not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-dd : text% dd -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-dd a-text a-dd)
  ("render-dd not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-dir : text% dir -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-dir a-text a-dir)
  ("render-dir not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-div : text% div -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-div a-text a-div)
  ("render-div not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-dfn : text% dfn -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-dfn a-text a-dfn)
  ("render-dfn not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-dl : text% dl -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-dl a-text a-dl)
  ("render-dl not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-dt : text% dt -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-dt a-text a-dt)
  ("render-dt not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-em : text% em -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-em a-text a-em)
  (local [(define G5 (first (html:html-full-content a-em)))
          (define initpos (send a-text get-start-position))]
    (render-G5 a-text G5)
    (italicize a-text initpos (send a-text get-start-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-fieldset : text% fieldset -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-fieldset a-text a-fieldset)
  ("render-fieldset not yet implfieldsetented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-font : text% font -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-font a-text a-font)
  (local [(define attributes (html:html-element-attributes a-font))
          (define G5 (first (html:html-full-content a-font)))
          (define initpos (send a-text get-start-position))]
    (render-G5 a-text G5)
    (for-each (lambda (attrib)
                (fontify a-text attrib initpos (send a-text get-start-position)))
              attributes)))           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-form : text% form -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-form a-text a-form)
  ("render-form not yet implformented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h1 : text% h1 -> void
; Renders h1 on a-text.  If G5 is sizable text, it will be 
; rendered with HeadingsFontSize 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h1 a-text a-h1)
  (local [(define h1-attribs (html:html-element-attributes a-h1))
          (define listof-G5 (html:html-full-content a-h1))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 0)))]
    (cond [(empty? listof-G5) (void)]
          [else (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
                (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h2 : text% h2 -> void
; Renders h2 on a-text.  If G5 is sizable text, it will be 
; rendered with HeadingsFontSize 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h2 a-text a-h2)
  (local [(define h2-attribs (html:html-element-attributes a-h2))
          (define listof-G5 (html:html-full-content a-h2))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 1)))]
    (cond [(empty? listof-G5) (void)]
          [else 
           (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
           (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h3 : text% h3 -> void
; Renders h3 on a-text.  If G5 is sizable text, it will be rendered
; with HeadingsFontSize 2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h3 a-text a-h3)
  (local [(define h3-attribs (html:html-element-attributes a-h3))
          (define listof-G5 (html:html-full-content a-h3))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 2)))]
    (cond [(empty? listof-G5) (void)]
          [else 
           (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
           (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h4 : text% h4 -> void
; Renders h4 on a-text.  If G5 is sizable text, it will be rendered
; with HeadingsFontSize 3.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h4 a-text a-h4)
  (local [(define h4-attribs (html:html-element-attributes a-h4))
          (define listof-G5 (html:html-full-content a-h4))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 3)))]
    (cond [(empty? listof-G5) (void)]
          [else 
           (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
           (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h5 : text% h5 -> void
; REnders h5 on a-text.  If G5 is sizable text, it will be rendered
; with HeadingsFontSize 4.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h5 a-text a-h5)
  (local [(define h5-attribs (html:html-element-attributes a-h5))
          (define listof-G5 (html:html-full-content a-h5))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 4)))]
    (cond [(empty? listof-G5) (void)]
          [else 
           (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
           (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-h6 : text% h6 -> void
; Renders H6 on a-text.  If G5 is sizable text, it will be rendered
; with HeadingsFontSize 5.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-h6 a-text a-h6)
  (local [(define h6-attribs (html:html-element-attributes a-h6))
          (define listof-G5 (html:html-full-content a-h6))
          (define initpos (send a-text get-start-position))
          (define HeadingSize (make-object style-delta% 'change-size (vector-ref HeadingSizesVector 5)))]
    (cond [(empty? listof-G5) (void)]
          [else 
           (for-each (lambda (G5)
                            (render-G5 a-text G5)) listof-G5)
           (send a-text change-style HeadingSize initpos (send a-text get-start-position))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-hr : text% hr -> void
; Renders the HR tag, a-hr, on a-text%.  Recognizes attributes
; ALIGN, SIZE (meaning height), WIDTH, NOSHADE, and COLOR.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-hr a-text a-hr)
  (local [(define hr-attribs (html:html-element-attributes a-hr))
          (define alignment (string->symbol (get-attribute-value hr-attribs 'align "left")))
          (define size (string->number (get-attribute-value hr-attribs 'size "1")))
          (define noshade? (if (empty? (filter (lambda (attrib)
                                                 (symbol=? (attribute-name attrib) 'noshade)) hr-attribs))
                               #f
                               #t))
          (define color-object (color-value->object (get-attribute-value hr-attribs 'color "black")))
          (define-values (text-width text-height) (send a-text get-max-view-size))
          (define width (length->pixels (get-attribute-value hr-attribs 'width "100%") text-width text-width))
          (define initpos (send a-text get-start-position))
          (define hr-snip (make-object hr-snip% size width color-object noshade?))]
    (send a-text insert hr-snip)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-html-object : text% object -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-html-object a-text a-html-object)
  ("render-html-object not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-i : text% i -> void
; Consumes a text object and renders G5 italicized on the text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-i a-text a-i)
  (local [(define G5 (first (html:html-full-content a-i)))
          (define initpos (send a-text get-start-position))]
    (render-G5 a-text G5)
    (italicize a-text initpos (send a-text get-start-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-iframe : text% iframe -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-iframe a-text a-iframe)
  ("render-iframe not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-img : text% img -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-img a-text a-img)
  (local [(define img-attribs (html:html-element-attributes a-img))
          (define src (get-attribute-value img-attribs 'src ""))
          (define base-path (string->url "file:///home/bonfield/testcases/render-html"))
          (define combined-path:url (combine-url/relative base-path src))
          (define filesuffix (get-file-suffix src))
          (define start (send a-text get-start-position))
          (define (reader i-p)
            (local [(define (read:listofchar)
                      (local [(define next-obj (read-char i-p))]
                        (cond [(eof-object? next-obj) empty]
                              [else (cons next-obj (read:listofchar))])))
                    (define input-listof-char (read:listofchar))
                    (define output-filename (make-temporary-file))
                    (define (writer)
                      (for-each write-char input-listof-char))]
              (with-output-to-file output-filename writer 'truncate)
              (send a-text insert
                    (make-object image-snip% output-filename 'unknown #f #t))
              (delete-directory/files output-filename)))
          (define (open-url)
            (local [(define scheme (url-scheme url))
                    (define fragment (url-fragment url))
                    (define path (url-path url))
                    (define suffix (get-file-suffix path))]
              (cond [(and (boolean? scheme)
                          )
                     (void)]
                    [(and (boolean? scheme)
                          (integer? target-location))
                     (send a-text scroll-to-position target-location #f (+ (send a-text last-position) target-location) 'start)]
                    [(and (string-ci=? scheme "http")
                          (empty? target-location))
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))]
                    [(and (string-ci=? scheme "http")
                          (integer? target-location))
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))
                     (send a-text scroll-to-position target-location #f (+ (send a-text last-position) target-location) 'start)]
                    [else 
                     (send a-text erase)
                     (render-html a-text (call/input-url url get-pure-port html:read-html))])))]
    (call/input-url combined-path:url get-pure-port reader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-input : text% input -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-input a-text a-input)
  ("render-input not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-ins : text% ins -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-ins a-text a-ins)
  ("render-ins not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-isindex : text% isindex -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-isindex a-text a-isindex)
  ("render-isindex not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-kbd : text% kbd -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-kbd a-text a-kbd)
  ("render-kbd not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-label : text% label -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-label a-text a-label)
  ("render-label not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-legend : text% legend -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-legend a-text a-legend)
  ("render-legend not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-li : text% li -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-li a-text a-li)
  ("render-li not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-link : text% link -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-link a-text a-link)
  ("render-link not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-map : text% map -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-map a-text a-map)
  ("render-map not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-menu : text% menu -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-menu a-text a-menu)
  ("render-menu not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-meta : text% meta -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-meta a-text a-meta)
  ("render-meta not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-noframes : text% noframes -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-noframes a-text a-noframes)
  ("render-noframes not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-noscript : text% noscript -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-noscript a-text a-noscript)
  ("render-noscript not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-ol : text% ol -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-ol a-text a-ol)
  ("render-ol not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-option : text% option -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-option a-text a-option)
  ("render-option not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-optgroup : text% optgroup -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-optgroup a-text a-optgroup)
  ("render-optgroup not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-p : text% p -> void
; To render a paragraph on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-p a-text a-p)
  (paragraphicize a-text a-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-param : text% param -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-param a-text a-param)
  ("render-param not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-pcdata : text% pcdata -> void
; Renders a-pcdata on the text using the current basefont.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-pcdata text a-pcdata)
  (define initpos (send text get-start-position))
  (local [(define start (send text last-position))
          (define loc (string->list (pcdata-string a-pcdata)))
          (define stripped-newline-loc (filter (lambda (achar)
                                                 (not (char=? #\newline achar)))
                                               loc))
          (define fresh-string (list->string stripped-newline-loc))]
    (send text insert fresh-string)
    (send text change-style base-style-delta start (send text last-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-pre : text% pre -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-pre a-text a-pre)
  ("render-pre not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-q : text% q -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-q a-text a-q)
  ("render-q not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-s : text% s -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-s a-text a-s)
  ("render-s not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-samp : text% samp -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-samp a-text a-samp)
  ("render-samp not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-script : text% script -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-script a-text a-script)
  ("render-script not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-select : text% select-> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-select a-text a-select)
  ("render-select not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-small : text% small -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-small a-text a-small)
  (local [(define howsmall -1)
          (define G5 (filter (lambda (x)
                               (G5? x))
                             (html:html-full-content a-small)))]
    (cond [(empty? G5) (void)]
          [else
           (render-G5-with-relative-size a-text (first G5) howsmall)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-span : text% span -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-span a-text a-span)
  ("render-span not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-strike : text% strike -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-strike a-text a-strike)
  ("render-strike not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-strong : text% strong -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-strong a-text a-strong)
  (local ((define G5 (first (html:html-full-content a-strong)))
          (define initpos (send a-text get-start-position)))
    (render-G5 a-text G5)
    (boldify a-text initpos (send a-text get-start-position))))
          
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-style : text% style -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-style a-text a-style)
  ("render-style not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-sub : text% sub -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-sub a-text a-sub)
  ("render-sub not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-sup : text% sup -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-sup a-text a-sup)
  ("render-sup not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-table : text% table -> void
; To render a table on a text. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SEE "render-table.ss"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-tbody : text% tbody -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-tbody a-text a-tbody)
  ("render-tbody not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-td : text% td -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-td a-text a-td)
  ("render-td not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-textarea : text% textarea -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-textarea a-text a-textarea)
  ("render-textarea not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-th : text% th -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-th a-text a-th)
  ("render-th not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-thead : text% thead -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-thead a-text a-thead)
  ("render-thead not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-title : text% title -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-title a-text a-title)
  ("render-title not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-tfoot : text% tfoot -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-tfoot a-text a-tfoot)
  ("render-tfoot not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-tr : text% tr -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-tr a-text a-tr)
  ("render-tr not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-u : text% u -> void
; Consumes a text and renders G5 underlined on the text object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-u a-text a-u)
  (local [(define G5 (first (html:html-full-content a-u)))
          (define initpos (send a-text get-start-position))]
    (render-G5 a-text G5)
    (underline a-text initpos (send a-text get-start-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-ul : text% ul -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-ul a-text a-ul)
  ("render-ul not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-var : text% var -> void
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-var a-text a-var)
  ("render-var not yet implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G2 : text% G2 -> void
; Renders G2 data by determining whether a-G2 is a form or G3,
; then calls th3 appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G2 a-text a-G2)
  (cond [(html:form? a-G2) (render-form a-text a-G2)]
        [(G3? a-G2) (render-G3 a-text a-G2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G3 : text% G3 -> void
; Renders G3 data by determining which one of fieldset, isindex,
; G4, or G11 it represents, then calls teh appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G3 a-text a-G3)
  (cond [(html:fieldset? a-G3) (render-fieldset a-text a-G3)]
        [(html:isindex? a-G3) (render-isindex a-text a-G3)]
        [(G4? a-G3) (render-G4 a-text a-G3)]
        [(G11? a-G3) (render-G11 a-text a-G3)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G4 : text% G4 -> void
; Renders G4 data by identifying it as G8 or G10 data, and then
; calling the appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G4 a-text a-G4)
  (cond [(G8? a-G4) (render-G8 a-text a-G4)]
        [(G10? a-G4) (render-G10 a-text a-G4)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G5 : text% G5 -> void
; Renders G5 data by identifying it as G6 or label, and then
; calling the appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G5 a-text a-G5)
  (cond [(html:label? a-G5) (render-label a-text a-G5)]
        [(G6? a-G5) (render-G6 a-text a-G5)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G5-with-relative-size : text% G5 num -> (void)
; Renders G5 on a-text between x and y.  If G5 is sizable text,
; it will be rendered with delta-size offset to basefontsize.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G5-with-relative-size a-text a-G5 delta-size)
  (local [(define initpos (send a-text get-start-position))
          (define (endpos) (send a-text get-start-position))
          (define newsize (cond [(and (> delta-size 0)
                                      (>= basefontsize (- (vector-length fontsizesvector) delta-size)))
                                 7]
                                [(> delta-size 0) (+ basefontsize delta-size)]
                                [(and (< delta-size 0)
                                      (>= basefontsize (add1 delta-size)))
                                 (+ basefontsize delta-size)]
                                [(< delta-size 0) 1]))]
    (render-G5 a-text a-G5)
    (changefontsize a-text (number->string newsize) initpos (endpos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G6 : text% G6 -> void
; Renders G6 data by identifying it as G7 data or a-struct,
; and then calls teh appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G6 a-text a-G6)
  (cond [(html:a? a-G6) (render-a a-text a-G6)]
        [(G7? a-G6) (render-G7 a-text a-G6)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G7 : text% G7 -> void
; Renders G7 data by identifying it as G8 or G12 data,
; and then calls teh appropriate function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G7 a-text a-G7)
  (cond [(G8? a-G7) (render-G8 a-text a-G7)]
        [(G12? a-G7) (render-G12 a-text a-G7)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G8 : text% G8 -> void
; Renders G8 data by identifying it and calling the appropriate
; function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G8 a-text a-G8)
  (cond [(html:applet? a-G8) (render-applet a-text a-G8)]
        [(html:basefont? a-G8) (render-basefont a-text a-G8)]
        [(html:big? a-G8) (render-big a-text a-G8)]
        [(html:font? a-G8) (render-font a-text a-G8)]
        [(html:img? a-G8) (render-img a-text a-G8)]
        [(html:object? a-G8) (render-html-object a-text a-G8)]
        [(html:small? a-G8) (render-small a-text a-G8)]
        [(html:sub? a-G8) (render-sub a-text a-G8)]
        [(html:sup? a-G8) (render-sup a-text a-G8)]
        [(G9? a-G8) (render-G9 a-text a-G8)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G9 : text% G9 -> void
; Renders G9 data by identifying it and calling the appropriate
; function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G9 a-text a-G9)
  (cond [(html:abbr? a-G9) (render-abbr a-text a-G9)]
        [(html:acronym? a-G9) (render-acronym a-text a-G9)]
        [(html:b? a-G9) (render-b a-text a-G9)]
        [(html:bdo? a-G9) (render-bdo a-text a-G9)]
        [(html:br? a-G9) (render-br a-text a-G9)]
        [(html:cite? a-G9) (render-cite a-text a-G9)]
        [(html:code? a-G9) (render-code a-text a-G9)]
        [(html:dfn? a-G9) (render-dfn a-text a-G9)]
        [(html:em? a-G9) (render-em a-text a-G9)]
        [(html:i? a-G9) (render-i a-text a-G9)]
        [(html:kbd? a-G9) (render-kbd a-text a-G9)]
        [(html:map? a-G9) (render-map a-text a-G9)]
        [(pcdata? a-G9) (render-pcdata a-text a-G9)]
        [(html:q? a-G9) (render-q a-text a-G9)]
        [(html:s? a-G9) (render-s a-text a-G9)]
        [(html:samp? a-G9) (render-samp a-text a-G9)]
        [(html:script? a-G9) (render-script a-text a-G9)]
        [(html:span? a-G9) (render-span a-text a-G9)]
        [(html:strike? a-G9) (render-strike a-text a-G9)]
        [(html:strong? a-G9) (render-strong a-text a-G9)]
        [(html:tt? a-G9) (render-tt a-text a-G9)]
        [(html:u? a-G9) (render-u a-text a-G9)]
        [(html:var? a-G9) (render-var a-text a-G9)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G10 : text% G10 -> void
; Renders G10 data by identifying it and calling the appropriate
; function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G10 a-text a-G10)
  (cond [(html:address? a-G10) (render-address a-text a-G10)]
        [(html:blockquote? a-G10) (render-blockquote a-text a-G10)]
        [(html:center? a-G10) (render-center a-text a-G10)]
        [(html:dir? a-G10) (render-dir a-text a-G10)]
        [(html:div? a-G10) (render-div a-text a-G10)]
        [(html:dl? a-G10) (render-dl a-text a-G10)]
        [(html:h1? a-G10) (render-h1 a-text a-G10)]
        [(html:h2? a-G10) (render-h2 a-text a-G10)]
        [(html:h3? a-G10) (render-h3 a-text a-G10)]
        [(html:h4? a-G10) (render-h4 a-text a-G10)]
        [(html:h5? a-G10) (render-h5 a-text a-G10)]
        [(html:h6? a-G10) (render-h6 a-text a-G10)]
        [(html:hr? a-G10) (render-hr a-text a-G10)]
        [(html:menu? a-G10) (render-menu a-text a-G10)]
        [(html:noframes? a-G10) (render-noframes a-text a-G10)]
        [(html:noscript? a-G10) (render-noscript a-text a-G10)]
        [(html:ol? a-G10) (render-ol a-text a-G10)]
        [(html:p? a-G10) (render-p a-text a-G10)]
        [(html:pre? a-G10) (render-pre a-text a-G10)]
        [(html:table? a-G10) (render-table a-text a-G10)]
        [(html:ul? a-G10) (render-ul a-text a-G10)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G11 : text% G11 -> void
; Renders G11 data by identifying it and calling the appropriate
; function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G11 a-text a-G11)
  (cond [(html:a? a-G11) (render-a a-text a-G11)]
        [(html:label? a-G11) (render-label a-text a-G11)]
        [(G12? a-G11) (render-G12 a-text a-G11)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-G12 : text% G12 -> void
; Renders G12 data by identifying it and calling the appropriate
; function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-G12 a-text a-G12)
  (cond [(html:button? a-G12) (render-button a-text a-G12)]
        [(html:iframe? a-G12) (render-iframe a-text a-G12)]
        [(html:input? a-G12) (render-input a-text a-G12)]
        [(html:select? a-G12) (render-select a-text a-G12)]
        [(html:textarea? a-G12) (render-textarea a-text a-G12)]))
