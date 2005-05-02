#cs
(module tile-info mzscheme 

  ;; -- encapsulate knowledge about indexes (tiles w/o locations)
  ;; -- prepare for publication on web

  (require "if.scm"
           "rotate.scm"
           (file "Testing/testing.scm")
           (lib "xml.ss" "xml")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss"))
  
  (provide (struct desc (content north east south west))
           lookup          ;; Index -> Desc
           ;; lookup a piece via its index 

           desc-ref      ;; Desc Direction -> SIDES
           ;; what is facing in the given direction 

           index-snip    ;; Index Orientation -> snip<%>
           ;; produce a graphical snip from the given index
           
           make-random-set-of-tiles ;; Natural-> Listof[Index]
           ;; make a random list of n tiles for one game 
           )
  
  (define (make-random-set-of-tiles n)
    (map number->string (build-list n (lambda (i) (add1 (random 24))))))
  
  ;; --- DATA DEFS --- 
  ;;   Piece = 
  ;;    `(piece ,STRING] [c ,CONTENT] [n ,SIDES] [e ,SIDES] [w ,SIDES] [s ,SIDES])
  ;;   Desc =
  ;;    (make-desc CONTENT SIDES SIDES SIDES SIDES)
  
  (define-struct desc (content north east south west) (make-inspector))
  
  (define SIDES   '("castle" "road" "grass"))
  (define CONTENT '("none" "abbey" "flag" "castle" "end-of-road"))
  
  ;; CONTENT : 
  ;; -- castle means the sides of the castle are connected 
  ;; -- flag means "castle" (s.a.) and the castle piece contains a flag
  ;; -- end-of-road means the road runs into an obstacle 

  (define (lookup index o)
    (let loop ([p (cddr pieces)])
      (cond
        [(null? p) (error 'lookup-attribute "piece not found ~s" index)]
        [(string=? (piece-src (car p)) index) 
         (let* ([p (car p)]
                [n (piece-north p)]
                [e (piece-east p)]
                [s (piece-south p)]
                [w (piece-west p)]
                [c (piece-con p)])
           (cond
             [(= o 0)   (make-desc c n e s w)]
             [(= o 90)  (make-desc c w n e s)]
             [(= o 180) (make-desc c s w n e)]
             [(= o 270) (make-desc c e s w n)]
             [else (error 'description-rotate "bad orientation: ~s" o)]))]
        [else (loop (cdr p))])))

  (define (desc-ref d dir)
    ([dir-case dir desc-north desc-east desc-south desc-west] d))

  ;; Symbol -> (Tile -> String)
  (define (piece-attribute x)
    (lambda (t)
      (if (eq? (car t) 'piece)
          (let* ([attributes (cadr t)]
                 [result (assq x attributes)])
            (if result (cadr result) (error 'piece-attribute "~a not found" x)))
          (error 'piece-attribute "not a piece: ~a" x))))
  
  (define piece-north (piece-attribute 'n))
  (define piece-south (piece-attribute 's))
  (define piece-east  (piece-attribute 'e))
  (define piece-west  (piece-attribute 'w))
  (define piece-src   (piece-attribute 'src))
  (define piece-con   (piece-attribute 'c))
  
  
  (define pieces  ;; this could in principle be read from an XML file 
    '(xml
      ()
      (piece ((c "none") (e "road") (n "castle") (s "grass") (src "00") (w "road")))
      (piece ((c "abbey") (e "grass") (n "grass") (s "grass") (src "1") (w "grass")))
      (piece ((c "abbey") (e "grass") (n "grass") (s "road") (src "2") (w "grass")))
      (piece ((c "flag") (e "castle") (n "castle") (s "castle") (src "3") (w "castle")))
      (piece ((c "castle") (e "castle") (n "castle") (s "grass") (src "4") (w "castle")))
      (piece ((c "flag") (e "castle") (n "castle") (s "grass") (src "5") (w "castle")))
      (piece ((c "castle") (e "castle") (n "castle") (s "road") (src "6") (w "castle")))
      (piece ((c "flag") (e "castle") (n "castle") (s "road") (src "7") (w "castle")))
      (piece ((c "castle") (e "castle") (n "castle") (s "grass") (src "8") (w "grass")))
      (piece ((c "flag") (e "castle") (n "castle") (s "grass") (src "9") (w "grass")))
      (piece ((c "none") (e "grass") (n "castle") (s "road") (src "10") (w "road")))
      (piece ((c "flag") (e "castle") (n "castle") (s "road") (src "11") (w "road")))
      (piece ((c "castle") (e "castle") (n "grass") (s "grass") (src "12") (w "castle")))
      (piece ((c "flag") (e "castle") (n "grass") (s "grass") (src "13") (w "castle")))
      (piece ((c "none") (e "castle") (n "castle") (s "grass") (src "14") (w "grass")))
      (piece ((c "none") (e "grass") (n "castle") (s "castle") (src "15") (w "grass")))
      (piece ((c "none") (e "grass") (n "castle") (s "grass") (src "16") (w "grass")))
      (piece ((c "none") (e "road") (n "castle") (s "road") (src "17") (w "grass")))
      (piece ((c "none") (e "grass") (n "castle") (s "road") (src "18") (w "road")))
      (piece ((c "end-of-road") (e "road") (n "castle") (s "road") (src "19") (w "road")))
      (piece ((c "none") (e "road") (n "castle") (s "grass") (src "20") (w "road")))
      (piece ((c "none") (e "grass") (n "road") (s "road") (src "21") (w "grass")))
      (piece ((c "none") (e "road") (n "grass") (s "road") (src "22") (w "grass")))
      (piece ((c "end-of-road") (e "road") (n "grass") (s "road") (src "23") (w "road")))
      (piece ((c "end-of-road") (e "road") (n "road") (s "road") (src "24") (w "road")))))
  
  ;; --- CONSISTENCY CHECK ---
  (define (pieces-check ts)    
    (if (set? (map piece-src ts))
        (andmap piece-check ts) 
        (printf "pieces-check: not a set of names: ~a" (map piece-src ts))))
  
  (define (set? X)
    (cond [(null? X) #t]
          [else (and (not (member (car X) (cdr X))) (set? (cdr X)))]))
  
  (define (piece-check t)
    (let ([n (piece-north t)]
          [s (piece-south t)]
          [e (piece-east t)]
          [w (piece-west t)]
          [c (piece-con t)])
      (define (p x) (printf "piece-check: ~s is bad in ~s~n" t x))
      (unless (member n SIDES) (p 'north))
      (unless (member s SIDES) (p 'south))
      (unless (member e SIDES) (p 'east))
      (unless (member w SIDES) (p 'west))
      (unless (member c CONTENT) (p 'content))
      #t))
  
  
  ;; --- HTML GENERATION ---  
  (define (pieces-html ts)
    (let ([title  "Tiles for Carcassonne"])
      (with-output-to-file (file->path "pieces.html")
        (lambda ()
          (display-xml/content
           (xexpr->xml 
            `(html
              (title ,title)
              (body 
               (h3 ,title)
               (blockquote
                (table 
                 ([border "1"])
                 (tr (th "Index") (th "FileName") 
                     (th "Image") (th "90") (th "180") (th "270"))
                 ,@(map piece-tr ts))))))))
        'replace)))
  
  
  (define (piece-tr t)
    (let* ([index    (piece-src t)]
           [name     (produce index 0)]
           [name90   (produce index 90)]
           [name180  (produce index 180)]
           [name270  (produce index 270)])
      (if (string=? index "00")
          `(tr (td ,index) (td ,name) (td (img ([src ,name][alt ,index]))))
          `(tr (td ,index) (td ,name)
               (td (img ([src ,name][alt ,index])))
               (td (img ([src ,name90])))
               (td (img ([src ,name180])))
               (td (img ([src ,name270])))))))
  
  (define (produce index o)
    (let* ([sn (index-snip index o)]
           [bm (send sn get-bitmap)]
           [file (string-append (number->string o) (index->file index))])
      (send bm save-file (file->path file)'jpeg 90)
      file))
  
  ;; --- Check and Generate HTML --- 
  (define (check-and-produce-html)
    (let ([ts (cddr pieces)])
      (if (pieces-check ts)
          (begin (pieces-html ts)
                 (printf "table of pieces checked and HTML generated~n"))
          (printf "pieces don't check out~n"))))
  
  ;; --- AUX --- 
  
  ;; Index Orientation -> snip%
  (define (index-snip index o)
    (define bm ((if (string=? index "00") (lambda (x) x) (orientation->rotation o))
                (make-object bitmap% (file->path (index->file index)))))
    (define dc (new bitmap-dc% (bitmap bm)))
    (define __ (when (string=? index "00")
                 (send dc set-pen (make-object pen% "red" 3 'solid))
                 (send dc set-brush (make-object brush% "white" 'transparent))
                 (send dc draw-rectangle 0 0 
                       (send bm get-width) (send bm get-height))))
    ; (set! bm (send dc get-bitmap))
    (send dc set-bitmap #f)
    (make-object image-snip% bm))
  
  (define SOURCE "Tiles")
  (define (file->path f) (build-path SOURCE f))
  (define (index->file i) (string-append "tile" i ".jpg"))

  #| TESTS
  
  ; (check-and-produce-html)
  
  (printf "testing pieces ... ~n")
  (test== (lookup "2" 0) (make-desc "abbey" "grass" "grass" "road" "grass"))
  (printf "done ... ~n")
   |#
  )
