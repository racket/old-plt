;; Mario Latendresse, 29 May 2000
;;
;; Reading Jar files. Not yet completed.


(define (read-jar file)
  (with-input-from-file file
    (lambda ()
      (read-jar2))) )

(define (read-jar2)
  (let loop ((signature (read-4bytes)))
    (case signature
      ((67324752) ;; Local file header
       (write (read-file))(newline)
       (loop (read-4bytes)))
      ((33639248) ;; Central directory structure
       (write (read-directory)) (newline))
      (else (display-ln "No local file and no directory found."))
      )))


(define (read-directory)
  (let loop ((signature 33639248) (s '()))
    (if (= signature 33639248) ;; file header
	(let* ((one-entry (read-one-entry-directory))
	       (signature (read-4bytes)))
	  (loop signature (cons one-entry s)))
	(if (= signature 101010256) ;; end of central directory
	    (reverse s)
	    (display-ln "Did not encounter the end of central directory")
	    ))))
	    

(define (read-one-entry-directory)
  (let* ((versionMadeBy      (read-2bytes))
	 (versionExtract     (read-2bytes))
	 (flags              (read-2bytes))
	 (comprMethod        (read-2bytes))
	 (fileTime           (read-2bytes))
	 (fileDate           (read-2bytes))
	 (crc                (read-4bytes))
	 (comprSize          (read-4bytes))
	 (uncomprSize        (read-4bytes))
	 (fileNameLength     (read-2bytes))
	 (extrafieldLength   (read-2bytes))
	 (fileCommentLength  (read-2bytes))
	 (diskNumberStart    (read-2bytes))
	 (internalAttributes (read-2bytes))
	 (externalAttributes (read-4bytes))
	 (offsetHeader       (read-4bytes))
	 (filename           (read-string fileNameLength))
	 (extraField         (read-string extrafieldLength))
	 (fileComment        (read-string fileCommentLength)))
    (list filename comprSize uncomprSize)
    ))

;; The signature has been read.
(define (read-file)
  (let* ((version      (read-2bytes))
	 (flags        (read-2bytes))
	 (comprMethod  (read-2bytes))
	 (fileTime     (read-2bytes))
	 (fileDate     (read-2bytes))
	 (crc          (read-4bytes))
	 (comprSize    (read-4bytes))
	 (uncomprSize  (read-4bytes))
	 (fileNameLength (read-2bytes))
	 (extrafieldLength (read-2bytes))
	 (filename      (read-string fileNameLength))
	 (extraField    (read-string extraFieldLength))
	 (contentCompr  (read-file-content comprSize))
	 (contentUncompr (uncompress contentCompr comprMethod))
	 )
    (if (word-bit-set? flags 3) ;; Is there a data descriptor?
	(let* ((dataSignature (read-4bytes))
	       (dataCrc       (read-4bytes))
	       (comprSize     (read-4bytes))
	       (uncomprSize   (read-4bytes)))
	  (list comprSize uncomprSize filename))
	(list comprSize uncomprSize filename)
    )))

(define (uncompress content method)
  (display method))

(define (read-file-content n)
  (read-string n))

(define (read-string n)
  (let loop ((i 0) (s '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-char) s))
	(list->string (reverse s)))))

(define (lire-mot)
  (mot (lire-byte) (lire-byte)))

(define (lire-long)
  (long-mot (lire-byte) (lire-byte) (lire-byte)  (lire-byte)))

(define (lire-byte) (char->integer (read-char)))

(define 2^32 (expt 2 32))
(define 2^31 (expt 2 31))
(define 2^24 (expt 2 24))
(define 2^15 (expt 2 15))
(define 2^16 (expt 2 16))
(define 2^8  (expt 2 8))

;; Little-endian
(define (long-mot a1 a2 a3 a4) (+ (* a4 2^24) (* a3 2^16) (* a2 2^8) a1))

;; Little-endian
(define (mot a1 a2) (+ (* a2 2^8) a1))

(define read-2bytes lire-mot)
(define read-4bytes lire-long)

;; Return #t if ibit of word is 1.
(define (word-bit-set? word ibit)(odd? (quotient word (expt 2 ibit))))


