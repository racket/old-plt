(module make-bitmap mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "contracts.ss"))
  
  (provide/contract
   (make-bitmap
    (case-> (string? . -> . ((is-a?/c area-container<%>) . -> .
                             (union (is-a?/c bitmap%) string?)))
            (string? string? . -> . ((is-a?/c area-container<%>) . -> .
                                     (union (is-a?/c bitmap%) string?))))))
  
  ;; a bitmap with an icon and text
  (define make-bitmap 
    (case-lambda 
      [(button-name) (make-bitmap 
                      (let ([capd (string-copy button-name)])
                        (string-set! capd 0 (char-upcase (string-ref capd 0)))
                        capd)
                      (build-path
                       (collection-path "icons")
                       (string-append button-name ".bmp")))]
      [(text filename)
       (lambda (area-container-window)
         (let*-values ([(outside-margin) 2]
                       [(middle-margin) 3]
                       [(font) (send area-container-window get-control-font)]
                       [(img-bitmap-dc img-width img-height)
                        (let ([mdc (make-object bitmap-dc%)]
                              [q (make-object bitmap% filename)])
                          (if (send q ok?)
                              (begin (send mdc set-bitmap q)
                                     (values mdc
                                             (send q get-width)
                                             (send q get-height)))
                              (let ([b (make-object bitmap% 1 1)])
                                (send mdc set-bitmap b)
                                (send mdc clear)
                                (values mdc 0 0))))]
                       [(width height descent leading)
                        (begin (send img-bitmap-dc set-scale 1 1)
                               (send img-bitmap-dc get-text-extent text font))]
                       [(new-width) (inexact->exact
                                     (floor
                                      (+ outside-margin
                                         img-width
                                         middle-margin
                                         width
                                         outside-margin)))]
                       [(new-height) (inexact->exact
                                      (floor (+ outside-margin
                                                (max img-height height)
                                                outside-margin)))]
                       [(bitmap-dc) (make-object bitmap-dc%)]
                       [(new-bitmap) (make-object bitmap% new-width new-height)])
           (cond
             [(or (= img-width 0)
                  (= img-height 0))
              text]
             [else
              (send* bitmap-dc
                (set-bitmap new-bitmap)
                (set-scale 1 1)
                (set-font font)
                (clear)
                (draw-text text (+ outside-margin img-width middle-margin)
                           (- (/ new-height 2) (/ height 2))))
              (let ([bm (send img-bitmap-dc get-bitmap)])
                (send img-bitmap-dc set-bitmap #f)
                (send bitmap-dc draw-bitmap
                      bm
                      outside-margin
                      (- (/ new-height 2) (/ img-height 2)))
                (send bitmap-dc set-bitmap #f)
                new-bitmap)])))]))
  )