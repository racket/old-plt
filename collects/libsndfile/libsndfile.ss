(module libsndfile mzscheme
  
  (require (lib "contract.ss")
           (lib "homo-vectors.ss" "homogeneous-vectors"))
  
  (define endian-ness-format?
    (symbols 'file 'little 'big 'cpu))

  (define major-format?
    (symbols 'wav 'aiff 'au 'raw 'paf 'svx 'nist 'voc 'ircam 'w64 'mat4 'mat5))
  
  (define minor-format?
    (symbols 'pcm-s8 'pcm-16 'pcm-24 'pcm-32 'pcm-u8 'float 'double 'ulaw 'alaw 
             'ima-adpcm 'ms-adpcm 'gsm610 'vox-adpcm 'g721-32 'g723-24 'g723-40
             'dwvw-12 'dwvw-16 'dwvw-24 'dwvw-n))
  
  (define sf-error-level
    (make-parameter 'all-conversions
                    (lambda (warning-level)
                      (case warning-level
                        [(all-conversions no-conversions) warning-level]
                        [else (error 'sf-warning-level 
                                     "expected (symbols all-conversions no-conversions), got: ~v"
                                     warning-level)]))))
  

  (define-struct sf-info (frames ; integer?
                          samplerate ; integer?
                          channels ; integer?
                          endian-ness-format ; endian-ness-format?
                          major-format ; major-format?
                          minor-format ; minor-format?
                          sections ; integer?
                          seekable ; boolean?
                          port)) ; sf-port?
  
  (define-values (prim:sf-open-for-read
                  prim:sf-open-for-write
                  prim:sf-seek
                  prim:sf-close
                  prim:sf-read-short
                  prim:sf-read-int
                  prim:sf-read-float
                  prim:sf-read-double
                  prim:sf-write-short
                  prim:sf-write-int
                  prim:sf-write-float
                  prim:sf-write-double
                  prim:sf-port?)
    (load-extension (build-path (collection-path "libsndfile") "compiled" "native" (system-library-subpath) "mzsndfile.so")))  
  
  (define sf-port? prim:sf-port?)
  
  (define (create-buffer-for-open-port info frames . init-val-list)
    (let* ([vector-size (* (sf-info-channels info) frames)]
           [maker (natural-maker (sf-info-minor-format info))])
      (if (null? init-val-list)
          (maker vector-size)
          (maker vector-size (car init-val-list)))))
  
  (define (sf-open-for-read path mode) 
    (let* ([result (prim:sf-open-for-read path mode)]
           [info-vec (cadr result)]
           [port (car result)])
      (make-sf-info-from-vec info-vec port)))
  
  (define (sf-open-for-write path samplerate channels endian-ness-format major-format minor-format)
    (let* ([result (prim:sf-open-for-write path samplerate channels (make-format-id endian-ness-format major-format minor-format))]
           [info-vec (cadr result)]
           [port (car result)])
      (make-sf-info-from-vec info-vec port)))
  
  (define (sf-seek info frames whence)
    (prim:sf-seek (sf-info-port info) frames whence))
  
  (define (sf-command . args)
    (error 'sf-command "not implemented yet"))
  
  (define (sf-close info) 
    (prim:sf-close (sf-info-port info)))
  
  (define (sf-read info homo-vec offset frames)
    (read-write-check info homo-vec offset frames "read")
    ((cond
       [(s16vector? homo-vec) prim:sf-read-short]
       [(s32vector? homo-vec) prim:sf-read-int]
       [(f32vector? homo-vec) prim:sf-read-float]
       [(f64vector? homo-vec) prim:sf-read-double]
       [else (error 'sf-read "unsupported vector type: ~v" homo-vec)])
     (sf-info-port info)
     homo-vec
     offset
     frames))
  
  (define (sf-write info homo-vec offset frames)
    (read-write-check info homo-vec offset frames "write")
    ((cond
       [(s16vector? homo-vec) prim:sf-write-short]
       [(s32vector? homo-vec) prim:sf-write-int]
       [(f32vector? homo-vec) prim:sf-write-float]
       [(f64vector? homo-vec) prim:sf-write-double]
       [else (error 'sf-write "unsupported vector type: ~v" homo-vec)])
     (sf-info-port info)
     homo-vec
     (* (sf-info-channels info) offset)
     frames))
  
  (define (read-write-check info homo-vec offset frames err-string)
    (let ([predicate (natural-predicate (sf-info-minor-format info))])
    (when (and (eq? (sf-error-level) 'no-conversions)
                 (not (predicate homo-vec)))
      (error 'read-write-check "in error level ~v, vector ~v cannot be used for minor format ~v" 
             (sf-error-level)
             homo-vec
             (sf-info-minor-format info))))
      (when (> (+ (* (sf-info-channels info) offset) (* frames (sf-info-channels info))) (homo-vector-length homo-vec))
        (error 'read-write-check "attempted ~v of ~v frames would run off the end of supplied vector" err-string frames)))

  ; leaving these out, for the moment:
  ;      sf_count_t  sf_read_raw      (SNDFILE *sndfile, void *ptr, sf_count_t bytes) ;
  ;      sf_count_t  sf_write_raw     (SNDFILE *sndfile, void *ptr, sf_count_t bytes) ;

  
  (define (make-sf-info-from-vec v port)
    (let-values ([(endian-ness-format major-format minor-format) 
                  (decompose-format-id (vector-ref v 3))])
      (make-sf-info (vector-ref v 0)
                    (vector-ref v 1)
                    (vector-ref v 2)
                    endian-ness-format
                    major-format
                    minor-format
                    (vector-ref v 4)
                    (vector-ref v 5)
                    port)))
  
  (define GAP (gensym))
  
  (define endian-ness-bit-mapping
    (vector 'file 'little 'big 'cpu))
  ; whoa ; I'm not taking this into account.
  
  (define major-bit-mapping
    (vector GAP 'wav 'aiff 'au 'raw 'paf 'svx 'nist 'voc GAP 'ircam 'w64 'mat4 'mat5))
  
  (define minor-bit-mapping
    (vector GAP 'pcm-s8 'pcm-16 'pcm-24 'pcm-32 'pcm-u8 'float 'double 
            GAP GAP GAP GAP GAP GAP GAP GAP 
            'ulaw 'alaw 'ima-adpcm 'ms-adpcm GAP GAP GAP GAP
            GAP GAP GAP GAP GAP GAP GAP GAP
            'gsm610 'vox-adpcm GAP GAP GAP GAP GAP GAP 
            GAP GAP GAP GAP GAP GAP GAP GAP
            'g721-32 'g723-24 'g723-40 GAP GAP GAP GAP GAP
            GAP GAP GAP GAP GAP GAP GAP GAP
            'dwvw-12 'dwvw-16 'dwvw-24 'dwvw-n))

  (define (make-format-id endian-ness major minor)
    (let* ([endian-ness-bits (arithmetic-shift (vector-find endian-ness endian-ness-bit-mapping) 28)]
           [major-bits (arithmetic-shift (vector-find major major-bit-mapping) 16)]
           [minor-bits (vector-find minor minor-bit-mapping)])
      (+ endian-ness-bits major-bits minor-bits)))  
  
  (define (decompose-format-id id)
    (values (vector-ref endian-ness-bit-mapping (bitwise-and (arithmetic-shift id -28) (- (expt 2 2) 1)))
            (vector-ref major-bit-mapping (bitwise-and (arithmetic-shift id -16) (- (expt 2 12) 1)))
            (vector-ref minor-bit-mapping (bitwise-and id (- (expt 2 16) 1)))))
  
  (define (vector-find val vec)
    (let ([len (vector-length vec)])
      (let loop ([count 0])
        (if (= count len)
            (error 'vector-find "couldn't find ~v in vector ~v" val vec)
            (if (eq? val (vector-ref vec count))
                count
                (loop (+ count 1)))))))
  
  (define (natural-maker minor-format)
    (case minor-format
      [(pcm-s8) make-s8vector]
      [(pcm-16) make-s16vector]
      [(pcm-32) make-s32vector]
      [(pcm-u8) make-u8vector]
      [(float) make-f32vector]
      [(double) make-f64vector]
      [else (error 'natural-maker "natural form not known for minor encoding format: ~v" minor-format)]))
  
  (define (natural-predicate minor-format)
    (case minor-format
      [(pcm-s8) s8vector?]
      [(pcm-16) s16vector?]
      [(pcm-32) s32vector?]
      [(pcm-u8) u8vector?]
      [(float) f32vector?]
      [(double) f64vector?]
      [else (error 'natural-predicate "natural vector type not known for minor encoding format: ~v" minor-format)]))
  
  
  
  (provide/contract 
   
   ; since mutators are not provided, the sf-info structure is implicitly immutable
   [sf-info? (-> any? boolean?)]
   [sf-info-frames (-> sf-info? integer?)]
   [sf-info-samplerate (-> sf-info? integer?)]
   [sf-info-channels (-> sf-info? integer?)]
   [sf-info-endian-ness-format (-> sf-info? endian-ness-format?)]
   [sf-info-major-format (-> sf-info? major-format?)]
   [sf-info-minor-format (-> sf-info? minor-format?)]
   [sf-info-sections (-> sf-info? integer?)]
   [sf-info-seekable (-> sf-info? boolean?)]
   
   [sf-error-level parameter?]
   
   [create-buffer-for-open-port (case->
                                 (-> sf-info? ; info
                                     integer? ; frames
                                     any) ; homo-vec
                                 (-> sf-info? ; info
                                     integer? ; frames
                                     number? ; initialize
                                     any))] ; homo-vec
   
   [sf-open-for-read (-> string? ; path
                         (symbols 'read 'readwrite) ; mode
                         sf-info?)]  ; result
   
   [sf-open-for-write (-> string? ; path
                          integer? ; samplerate
                          integer? ; channels
                          endian-ness-format? ; endian-ness-format
                          major-format? ; major-format
                          minor-format? ; minor-format
                          sf-info?)] ; result
   
   [sf-seek (-> sf-info? ; info
                integer? ; frames
                (symbols 'from-beginning 'from-current 'from-end) ; whence
                void?)] ; result
   
   [sf-command (-> sf-info? ; info
                   (symbols 'unknown) ; don't know what goes here...?
                   string? ; data
                   integer? ; datasize ... can use length of string?
                   void?)] ; result
   
   [sf-close (-> sf-info? ; info
                 void?)] ; result
   
   [sf-read (-> sf-info? ; info
                any? ; storage
                integer? ; storage offset
                integer? ; frames
                void?)] ; result
   
   [sf-write (-> sf-info? ; info
                 any? ; storage
                 integer? ; storage offset
                 integer? ; frames
                 void?)] ; result
   
   ; leaving out read_raw & write_raw, for the moment.  Not entirely sure how to set them up.
   ))