(require "prompt.ss")

(define the-r-channel (make-channel))

(define my-frame
  (let ([new-es (make-eventspace)])
    (parameterize ([current-eventspace new-es])
      (new prompt-frame% (label "My Frame") (result-channel the-r-channel)))))

(send my-frame show #t)

(define (test qtn . args)
  (send my-frame prompt qtn . args)
  (apply values
         (channel-get the-r-channel)))

(test "how old are you?" 'foo)
(test "how long is a mile?" 'bar)

