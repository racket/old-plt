;; Mario Latendresse, 20 Mai 1998
;;                    23 May 2000
;;
;; Reading and writing .class files. This file was built using
;; lireclass.scm from my Ph.D. work.
;;

(define (read-class-dir dir f)
  (with-input-from-file (++dir dir f)
    (lambda ()
      (let* ((magic     (read-long)))
	(if (not (= magic #xCAFEBABE))
	    (begin 
	      (error "The magic number " magic " is incorrect. The file "
		     (string-append dir f) " is probably not a class file.")
	      #f)
	    (read-class magic))))
    ))

(define (read-class magic)
  (let* ((minor-version    (read-word))
	 (major-version    (read-word))
	 (cst-pool-cnt     (read-word))
	 (cst-pool         (read-constant-pool cst-pool-cnt))
	 (access_flags     (read-word))
	 (this-class       (read-word))
	 (super-class      (read-word))
	 (interfaces-count (read-word))
	 (interfaces       (read-interfaces interfaces-count))
	 (fields-count     (read-word))
	 (fields           (read-fields fields-count cst-pool))
	 (methods-count    (read-word))
	 (methods          (read-methods methods-count cst-pool))
	 (attributes-count (read-word))
	 (attribute-info   (read-attributes attributes-count cst-pool)))
    (list magic minor-version major-version cst-pool access_flags
	  this-class super-class interfaces
	  fields methods attribute-info)))

(define (read-methods n cst-pool)
  (define (read-method-info)
    (let* ((access-flags      (read-word))
	   (name-index        (read-word))
	   (descriptor-index  (read-word))
	   (attributes-count  (read-word))
	   (attributes        (read-attributes attributes-count cst-pool)))
      (list access-flags name-index descriptor-index attributes-count
	    attributes)
      ))

  ;;(display-ln "Lecture de " n " methods.")

  (let loop ((i 0) (r '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-method-info) r))
	(reverse r))))

(define (read-fields n cst-pool)

  (define (read-field-info)
    (let* ((access-flags      (read-word))
	   (name-index        (read-word))
	   (descriptor-index  (read-word))
	   (attributes-count  (read-word))
	   (attributes        (read-attributes attributes-count cst-pool)))
      (list access-flags name-index descriptor-index attributes-count
	    attributes)))

  ;;(display-ln "Lecture de " n " fields.")
  (let loop ((i 0) (r '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-field-info) r))
	(reverse r))))

(define (read-attributes n cst-pool)

  (define (read-info attribute-name-index n)
    (let* ((cst-name (value-cst-pool cst-pool attribute-name-index))
	   (name     (cadr cst-name))
	   (tag      (car cst-name)))
      
      (if (not (eqv? tag 1))
	  (error "Le nom de l'attribut n'est pas une chaîne utf "))
      (cond ((string-utf=? name "ConstantValue")
	     (read-attribute-constantvalue))
	    ((string-utf=? name "Code")
	     (read-attribute-code n cst-pool))
	    ((string-utf=? name "Exceptions")
	     (read-attribute-exception n))
	    (else
	     ; (display-ln "Attention, attribut inconnu " name)
	     (read-attribute-unknown n))
	    )))
	  
  (define (read-attribute-info)
    (let*  ((attribute-name-index (read-word))
	    (attribute-length     (read-long))
	    (info                 (read-info attribute-name-index
					     attribute-length)))
      (list attribute-name-index attribute-length info)))
  
  (let loop ((i 0) (r '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-attribute-info) r))
	(reverse r))))

(define (value-cst-pool cst-pool i) (vector-ref cst-pool i))

;; TBF. Not yet correct to compare utf8 strings.
(define (string-utf=? constant s) (string=? (cadr constant) s))

;; Dans le cas où l'attribut est inconnu, il faut simplement lire
;; les n octets.
(define (read-attribute-unknown n)
  (let loop ((i 0) (r '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-byte) r))
	(cons 'unknown (reverse r)))))

(define (read-attribute-code n cst-pool)

  ;; The bytecode is read here.

  (define (read-code n)
    (let loop ((i 0) (r '()))
      (if (< i n)
	  (loop (+ i 1) (cons (read-byte) r))
	  (reverse r))))

  (define (read-exception-table n)
    (define (read-exception)
      (let* ((start-pc   (read-word))
	     (end-pc     (read-word))
	     (handler-pc (read-word))
	     (catch-type (read-word)))
	(list start-pc end-pc handler-pc catch-type)
	))

    (let loop ((i 0) (r '()))
      (if (< i n)
	  (loop (+ i 1) (cons (read-exception) r))
	  (reverse r))))

  (let* ((max-stack              (read-word))
	 (max-locals             (read-word))
	 (code-length            (read-long))
	 (code                   (read-code code-length))
	 (exception-table-length (read-word))
	 (exception-table        (read-exception-table exception-table-length))
	 (attributes-count       (read-word))
	 (attributes             (read-attributes attributes-count cst-pool)))
    (list 'code
	  max-stack max-locals
	  code-length code exception-table-length exception-table
	  attributes-count attributes)))

(define (read-attribute-exception n)

  (define (read-exceptions n)
    (let loop ((i 0) (r '()))
      (if (< i n)
	  (loop (+ i 1) (cons (read-word) r))
	  (reverse r))))

  (let* ((number-of-exceptions  (read-word))
	 (exception-index-table (read-exceptions number-of-exceptions)))
    (list 'exception  number-of-exceptions exception-index-table)))

(define (read-attribute-constantvalue)
  (let* ((constantvalue-index   (read-word)))
    (list 'constantvalue constantvalue-index)))

;; O : [int]
(define (read-interfaces n)
  (let loop ((i 0) (r '()))
    (if (< i n)
	(loop (+ i 1) (cons (read-word) r))
	(reverse r))))

(define (read-constant-pool n)
  (let loop ((i 1) (r (make-vector n 'constante-vide)))
    (if (< i n)
	(let ((tag (read-byte)))
	  (case tag
	    ((7)  
	     (vector-set! r i (list tag (read-constant-class)))
	     (loop (+ i 1) r))
	    ((9)  
	     (vector-set! r i (list tag (read-constant-fieldref)))
	     (loop (+ i 1) r))
	    ((10)
	     (vector-set! r i (list tag (read-constant-methodref)))
 	     (loop (+ i 1) r))
	    ((11)
	     (vector-set! r i (list tag (read-constant-interface)))
 	     (loop (+ i 1) r))
	    ((8)  
	     (vector-set! r i (list tag (read-constant-string)))
	     (loop (+ i 1) r))
	    ((3)  	     
	     (vector-set! r i (list tag (read-constant-integer)))
	     (loop (+ i 1) r))
	    ((4)
	     (vector-set! r i (list tag (read-constant-float)))
  	     (loop (+ i 1) r))
	    ((5)  
	     ;; cas spécial où il faut compter deux entrées pour une constante.
	     (vector-set! r i (list tag (read-constant-long)))
	     (vector-set! r (+ i 1) 'unusable)
	     (loop (+ i 2) r))
	    ((6)  
	     ;; cas spécial où il faut compter deux entrées pour une constante.
	     (vector-set! r i (list tag (read-constant-double)))
	     (vector-set! r (+ i 1) 'unusable)
	     (loop (+ i 2)  r)) 
	    ((12) 
	     (vector-set! r i (list tag (read-constant-nameandtype)))
	     (loop (+ i 1) r))
	    ((1)  
	     (vector-set! r i (list tag (read-constant-utf8)))
	     (loop (+ i 1) r))
	    (else (error "Unknown Tag" tag r))))
	r)))

(define (read-constant-string)
  (let ((string-index (read-word)))
    string-index))

;; O : (list n str)
(define (read-constant-utf8) 
  (let ((n (read-word)))
    (let loop ((i 0) (s '()))
      (if (< i n)
	  (let ((first-byte (read-byte)))
	    (cond ((< first-byte 128)
		   (loop (+ i 1) (cons (integer->char first-byte) s)))
		  ((and (>= first-byte 128) (< first-byte 192))
		   (error "read-constant-utf8, unknown code " first-byte))
		  ((and (>= first-byte 192) (< first-byte 224))
		   (let* ((second-byte (read-byte))
			  (valeur (+ (* (- first-byte 192) 64) 
				     (- second-byte 128))))
		     ;; TBF: does not produce character with value over 255.
		     (if (> valeur 255)
			 (loop (+ i 2) (cons (integer->char 255) s))
			 (loop (+ i 2) (cons (integer->char valeur) s)))))
		  (else
		   (let* ((second-byte (read-byte))
			  (third-byte  (read-byte))
			  (valeur      (+  (* (- first-byte 224) 4096)
					   (* (- second-byte 128) 64)
					   (- third-byte 128))))
		     ;; TBF Should be modified to handle a value larger then 255 and
		     ;; not simply force it to 255 as it is coded now.
		     (if (> valeur 255)
			 (loop (+ i 3) (cons 255 s))
			 (loop (+ i 3) (cons (integer->char valeur) s)))))))
	  (list n (list->string (reverse s))))
      )))


(define (read-constant-class)
  (let ((name-index (read-word)))
    name-index))

(define (read-constant-fieldref)
  (let* ((class-index          (read-word))
	 (name-and-type-index  (read-word)))
    (list class-index name-and-type-index)))

(define (read-constant-methodref)
  (let* ((class-index          (read-word))
	 (name-and-type-index  (read-word)))
    (list class-index name-and-type-index)))

(define (read-constant-interface)
  (let* ((class-index          (read-word))
	 (name-and-type-index  (read-word)))
    (list class-index name-and-type-index)))

(define (read-constant-integer)
  (read-long))

(define (read-constant-float)
  (read-long))

(define (read-constant-long)
  (let ((partie-haute (read-long))
	(partie-basse (read-long)))
    (list partie-haute partie-basse)))

(define (read-constant-double)
  (let ((partie-haute (read-long))
	(partie-basse (read-long)))
    (list partie-haute partie-basse)))

(define (read-constant-nameandtype)
  (let* ((name-index       (read-word))
	 (descriptor-index (read-word)))
    (list name-index descriptor-index)))

(define (read-word)
  (mot (read-byte) (read-byte)))

(define (read-long)
  (long-mot (read-byte) (read-byte) (read-byte) (read-byte)))

;; There should be only one read-char in this entire file, and it
;; is in this function.
(define (read-byte)
  (let* ((c (read-char)))
    (if (eof-object? c)
	(error "End of file encountered prematurely."))
    (char->integer c)))

(define 2^32 (expt 2 32))
(define 2^31 (expt 2 31))
(define 2^24 (expt 2 24))
(define 2^15 (expt 2 15))
(define 2^16 (expt 2 16))
(define 2^8  (expt 2 8))

(define (long-mot a1 a2 a3 a4) (+ (* a1 2^24) (* a2 2^16) (* a3 2^8) a4))

(define (mot a1 a2)  (+ (* a1 2^8) a2))

;; To read the Java bytecode, the instruction length must be known.
;;
;; Entrée : l, list of bytes.
;;          offset, entier spécifiant l'adresse relative du premier octet
;;                  de la liste l. Cette valeur est nécessaire pour découvrir
;;                  le nombre de zéros-alignement des instructions tableswitch
;;                  et lookupswitch.
;; Sortie : paire (insts addresses) 
;;                liste d'instructions (mne opérandes)
;;          la liste des addresses donne l'adresse relative en octet de chaque
;;          instruction.
;;
(define (java-bytes->instruction l)
  (let loop ((r '()) (addrs '()) (offset 0) (l l))
  (if (pair? l) 
  (let* ((first-byte (car l))
	 (mne        (byte->java-mne first-byte))) 
    (case mne
      ((aaload aastore aconst_null aload_0 aload_1 aload_2 aload_3 areturn 
	       arraylength astore_0 astore_1 astore_2 astore_3 athrow
	       baload bastore caload castore d2f d2i d2l dadd daload
	       dastore dcmpg dcmpl dconst_0 dconst_1 ddiv dload_0 dload_1
	       dload_2 dload_3 dmul dneg drem dreturn dstore_0 dstore_1
	       dstore_2 dstore_3 dsub dup dup_x1 dup_x2 dup2 dup2_x1 dup2_x2
	       f2d f2i f2l fadd faload fastore fcmpg fcmpl fconst_0
	       fconst_1 fconst_2 fdiv fload_0 fload_1 fload_2 fload_3
	       fmul fneg frem freturn fstore_0 fstore_1 fstore_2 fstore_3
	       fsub i2b i2c i2d i2f i2l i2s iadd iaload iand iastore
	       iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4
	       iconst_5 idiv iload_0 iload_1 iload_2 iload_3 imul ineg
	       ior irem ireturn ishl ishr istore_0 istore_1 istore_2
	       istore_3 isub iushr ixor l2d l2f l2i ladd laload land
	       lastore lcmp lconst_0 lconst_1 ldiv lload_0 lload_1
	       lload_2 lload_3 lmul lneg lor lrem lreturn lshl lshr
	       lstore_0 lstore_1 lstore_2 lstore_3 lsub lushr lxor
	       monitorenter monitorexit nop pop pop2 return saload
	       sastore swap
	       )
       (loop (cons `(,mne) r) (cons offset addrs) (+ offset 1) (cdr l)))
      ;; Cas un octet non-signé.
      ((aload astore dload dstore fload fstore iload istore ldc lload
	      lstore newarray ret) 
       (if (null? (cdr l))
	   (error "bytes->instruction, c'est une instruction avec index, mais il n'y a pas d'index suivant l'mne" mne l))
       (let ((index (cadr l)))
	 (loop
	  (cons (list mne index) r)
	  (cons offset addrs)
	  (+ offset 2)
	  (cddr l)
	  )))

      ;; Cas un octet signé.
      ((bipush)
       (if (null? (cdr l))
	   (error "bytes->instruction, c'est une instruction avec index, mais il n'y a pas d'index suivant l'mne" mne l))
       (let ((index (1byte->c2 (cadr l))))
	 (loop
	  (cons (list mne index) r)
	  (cons offset addrs)
	  (+ offset 2)
	  (cddr l)
	  )))

      ;; Deux octets formant un nombre 16 bits non-signé.
      ((anewarray checkcast getfield getstatic 
		  instanceof invokespecial invokestatic invokevirtual
		  ldc_w ldc2_w new putfield putstatic)
 
       (if (or (null? (cdr l)) (null? (cddr l)))
	   (error "bytes->instruction, c'est une instruction avec index de 16 bits, mais il n'y a pas d'index de 16 bits suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index  (+ (* index1 2^8) index2)))
	 (loop (cons (list mne index) r)
	       (cons offset addrs)
	       (+ offset 3)
	       (cdddr l))))

      ;; Deux octets formant un nombre de 16 bits signé.
      ((goto if_acmpeq if_acmpne if_icmpeq if_icmpne if_icmplt
	     if_icmpge if_icmpgt if_icmple ifeq ifne iflt ifge
	     ifgt ifle ifnonnull  ifnull jsr sipush) 
       (if (or (null? (cdr l)) (null? (cddr l)))
	   (error "bytes->instruction, c'est une instruction avec index de 16 bits, mais il n'y a pas d'index de 16 bits suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index  (2bytes->c2 index1 index2))
	      )
	 (loop (cons (list mne index) r)
	       (cons offset addrs)
	       (+ offset 3)
	       (cdddr l))))
      ((iinc)
       (if (or (null? (cdr l)) (null? (cddr l)))
	   (error "bytes->instruction, c'est une instruction avec deux indices de 8 bits, mais il n'y a pas deux octets suivants l'mne" mne ", reste"  l) )
       (let* ((index1 (cadr l))
	      (const  (1byte->c2 (caddr l)))
	      )
	 (loop (cons (list mne index1 const) r)
	       (cons offset addrs)
	       (+ offset 3)
	       (cdddr l))))
      ((multianewarray)
       (if (or (null? (cdr l)) (null? (cddr l)) (null? (cdddr l)))
	   (error "bytes->instruction, c'est une instruction avec index de 16 bits et une dimension de 8 bits, mais il n'y a pas trois octets suivant l'mne" mne "reste"  l))
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index  (+ (* index1 2^8) index2))
	      (dimensions (cadddr l)))
	 (loop (cons (list mne index dimensions) r)
	       (cons offset addrs)
	       (+ offset 4)
	       (cddddr l))))
      ((goto_w jsr_w)
       (if (or (null? (cdr l)) (null? (cddr l)) (null? (cdddr l)) 
	       (null? (cddddr l)))
	   (error "bytes->instruction, c'est une instruction avec index de 32 bits, mais il n'y a pas d'index de 32 bits suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index3 (cadddr l))
	      (index4 (cadddr (cdr l)))
	      (index  (4bytes->c2 index1 index2 index3 index4))
	      )
	 (loop (cons (list mne index) r)
	       (cons offset addrs)
	       (+ offset 5)
	       (cddddr (cdr l)))))
      ((invokeinterface)
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index  (+ (* index1 256) index2))
	      (count  (cadddr l))
	      (zero   (cadddr (cdr l))))
	 (loop (cons (list mne index count zero) r)
	       (cons offset addrs)
	       (+ offset 5)
	       (cddddr (cdr l)))))
      ((lookupswitch)
       (let* ((padding (case (remainder (+ offset 1) 4)
			 ((0) '())
			 ((1) '(0 0 0))
			 ((2) '(0 0))
			 ((3) '(0))))
	      (lp  (case (remainder (+ offset 1) 4)
		     ((0) (cdr l))
		     ((1) (cddddr l))
		     ((2) (cdddr l))
		     ((3) (cddr l))))
	      (default (4bytes->c2 (car lp) (cadr lp) 
				   (caddr lp) (cadddr lp)))
	      (lpp    (cddddr lp))
	      (npairs    (4bytes->c2 (car lpp)(cadr lpp)
				     (caddr lpp) (cadddr lpp)))
	      (lppp   (cddddr lpp))
	      (pairs  (let loop ((i 1) (s lppp) (r '()))
			(if (<= i npairs)
			    (loop (+ i 1)
				  (cddddr (cddddr s))
				  (cons 
				   (cons 
				    (4bytes->c2 (car s) (cadr s)
						(caddr s) (cadddr s))
				    (4bytes->c2 (car (cddddr s))  
						(cadr (cddddr s))
						(caddr (cddddr s)) 
						(cadddr (cddddr s))))
				   r))
			    (cons (reverse r) s)))))	      
	 (loop (cons (list mne padding default npairs (car pairs)) r)
	       (cons offset addrs)
	       (+ offset 1 (length padding) 8 (* npairs 8))
	       (cdr pairs))))
      ((tableswitch)
       (let* ((padding (case (remainder (+ offset 1) 4)
			 ((0) '())
			 ((1) '(0 0 0))
			 ((2) '(0 0))
			 ((3) '(0))))
	      (lp  (case (remainder (+ offset 1) 4)
		     ((0) (cdr l))
		     ((1) (cddddr l))
		     ((2) (cdddr l))
		     ((3) (cddr l))
		     ))
	      (default (4bytes->c2 (car lp) (cadr lp) (caddr lp)
				   (cadddr lp)))
	      (lpp    (cddddr lp))
	      (low    (4bytes->c2 (car lpp) (cadr lpp) (caddr lpp)
				  (cadddr lpp)))
	      (lppp   (cddddr lpp))
	      (high   (4bytes->c2 (car lppp) (cadr lppp) (caddr lppp)
				  (cadddr lppp)))
	      (jumps  (let loop ((i low) (s (cddddr lppp)) (r '()))
			(if (<= i high)
			    (loop (+ i 1)
				  (cddddr s)
				  (cons (4bytes->c2 (car s) (cadr s)
						    (caddr s) (cadddr s))
					r))
			    (cons (reverse r) s)))))
	 (loop (cons (list mne padding default low high (car jumps)) r)
	       (cons offset addrs)
	       (+ offset 1 (length padding) 12 (* 4 (- high low -1)))
	       (cdr jumps))))
      ((wide)
       (let ((op-code2 (cadr l)))
	 (if (= op-code2 132)
	     (let*  ((index1  (caddr l))
		     (index2  (cadddr l))
		     (index   (+ (* index1 256) index2))
		     (const1  (cadddr (cdr l)))
		     (const2  (cadddr (cddr l)))
		     (const   (2bytes->c2 index1 index2))
		     )
	       (loop (cons (list 'wide 132 index const) r)
		     (cons offset addrs)
		     (+ offset 6)
		     (cddddr (cddr l))))
	     (begin
	       (if (not (member (byte->java-mne op-code2)
				'(iload fload aload lload
					dload istore fstore astore 
					lstore dstore ret)))
		   (error "L'instruction wide n'est pas suivie d'une instruction admissible : " op-code2))
	       (let*  ((index1  (caddr l))
		       (index2  (cadddr l))
		       (index   (+ (* index1 256) index2))
		       )
		 (loop (cons (list 'wide op-code2 index) r)
		       (cons offset addrs)
		       (+ offset 4)
		       (cddddr l)))))
	 ))
      (else (error "bytes->instruction, code opérationnel inconnu " mne))))
  (cons (reverse r) (reverse addrs)))
  ))


;; Similaire à bytes->instruction mais où les opérandes des instructions
;; sont laissées sous la forme d'une liste d'octets.
;;
;; Entrée : l, liste d'octets
;;          offset, entier spécifiant l'adresse relative du premier octet
;;                  de la liste l. Cette valeur est nécessaire pour découvrir
;;                  le nombre de zéros-alignement des instructions tableswitch
;;                  et lookupswitch.
;; Sortie : liste d'instructions (mne opérandes-octets)

(define (java-bytes->mne-octets l)
  (let loop ((r '()) (offset 0) (l l))
  (if (pair? l) 
  (let* ((first-byte (car l))
	 (mne    (byte->java-mne first-byte))) 
    (case mne
      ((aaload aastore aconst_null aload_0 aload_1 aload_2 aload_3 areturn 
	       arraylength astore_0 astore_1 astore_2 astore_3 athrow
	       baload bastore caload castore d2f d2i d2l dadd daload
	       dastore dcmpg dcmpl dconst_0 dconst_1 ddiv dload_0 dload_1
	       dload_2 dload_3 dmul dneg drem dreturn dstore_0 dstore_1
	       dstore_2 dstore_3 dsub dup dup_x1 dup_x2 dup2 dup2_x1 dup2_x2
	       f2d f2i f2l fadd faload fastore fcmpg fcmpl fconst_0
	       fconst_1 fconst_2 fdiv fload_0 fload_1 fload_2 fload_3
	       fmul fneg frem freturn fstore_0 fstore_1 fstore_2 fstore_3
	       fsub i2b i2c i2d i2f i2l i2s iadd iaload iand iastore
	       iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4
	       iconst_5 idiv iload_0 iload_1 iload_2 iload_3 imul ineg
	       ior irem ireturn ishl ishr istore_0 istore_1 istore_2
	       istore_3 isub iushr ixor l2d l2f l2i ladd laload land
	       lastore lcmp lconst_0 lconst_1 ldiv lload_0 lload_1
	       lload_2 lload_3 lmul lneg lor lrem lreturn lshl lshr
	       lstore_0 lstore_1 lstore_2 lstore_3 lsub lushr lxor
	       monitorenter monitorexit nop pop pop2 return saload
	       sastore swap)
       (loop (cons `(,mne) r) (+ offset 1) (cdr l)))
      ((aload astore bipush dload dstore fload fstore iload istore ldc lload
	      lstore newarray ret) 
       (if (null? (cdr l))
	   (error "bytes->mne-octets, c'est une instruction avec index, mais il n'y a pas d'index suivant l'op-code" mne l))
       (let ((index (cadr l)))
	 (loop
	  (cons (list mne index) r)
	  (+ offset 2)
	  (cddr l))))
      ((anewarray checkcast getfield getstatic goto if_acmpeq if_acmpne
		  if_icmpeq  if_icmpne  if_icmplt  if_icmpge  if_icmpgt
		  if_icmple ifeq ifne iflt ifge ifgt ifle ifnonnull ifnull
		  instanceof invokespecial invokestatic invokevirtual
		  jsr ldc_w ldc2_w new putfield putstatic sipush)
       (if (or (null? (cdr l)) (null? (cddr l)))
	   (error "bytes->mne-octets, c'est une instruction avec index de 16 bits, mais il n'y a pas d'index de 16 bits suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l)))
	 (loop (cons (list mne index1 index2) r)
	       (+ offset 3)
	       (cdddr l))))
      ((iinc)
       (if (or (null? (cdr l)) (null? (cddr l)))
	   (error "bytes->mne-octets, c'est une instruction avec deux indices de 8 bits, mais il n'y a pas deux octets suivants l'mne" mne ", reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l)))
	 (loop (cons (list mne index1 index2) r)
	       (+ offset 3)
	       (cdddr l))))
      ((multianewarray)
       (if (or (null? (cdr l)) (null? (cddr l)) (null? (cdddr l)))
	   (error "bytes->mne-octets, c'est une instruction avec index de 16 bits et une dimension de 8 bits, mais il n'y a pas trois octets suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (dimensions (cadddr l)))
	 (loop (cons (list mne index1 index2 dimensions) r)
	       (+ offset 4)
	       (cddddr l))))
      ((goto_w jsr_w)
       (if (or (null? (cdr l)) (null? (cddr l)) (null? (cdddr l)) 
	       (null? (cddddr)))
	   (error "bytes->mne-octets, c'est une instruction avec index de 32 bits, mais il n'y a pas d'index de 32 bits suivant l'mne" mne "reste"  l) )
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (index3 (cadddr l))
	      (index4 (cadddr (cdr l))))
	 (loop (cons (list mne index1 index2 index3 index4) r)
	       (+ offset 5)
	       (cddddr (cdr l)))))
      ((invokeinterface)
       (let* ((index1 (cadr l))
	      (index2 (caddr l))
	      (count  (cadddr l))
	      (zero   (cadddr (cdr l))))
	 (loop (cons (list mne index1 index2 count zero) r)
	       (+ offset 5)
	       (cddddr (cdr l)))))
      ((lookupswitch)
       (let* ((padding (case (remainder (+ offset 1) 4)
			 ((0) '())
			 ((1) '(0 0 0))
			 ((2) '(0 0))
			 ((3) '(0))))
	      (lp  (case (remainder (+ offset 1) 4)
		     ((0) (cdr l))
		     ((1) (cddddr l))
		     ((2) (cdddr l))
		     ((3) (cddr l))
		     ))
	      (default  (4bytes->c2 (car lp) (cadr lp) (caddr lp) (cadddr lp))) 
	      (lpp    (cddddr lp))
	      (npairs    (4bytes->c2 (car lpp) (cadr lpp) (caddr lpp)
				     (cadddr lpp)))
	      (longueur (+ (length padding) 8 (* npairs 8))))
	 (loop (cons `(,mne ,@(list-head (cdr l) longueur)) r)
	       (+ offset longueur 1)
	       (list-tail (cdr l) longueur))))
      ((tableswitch)
       (let* ((padding (case (remainder (+ offset 1) 4)
			 ((0) '())
			 ((1) '(0 0 0))
			 ((2) '(0 0))
			 ((3) '(0))))
	      (lp  (case (remainder (+ offset 1) 4)
		     ((0) (cdr l))
		     ((1) (cddddr l))
		     ((2) (cdddr l))
		     ((3) (cddr l))
		     ))
	      (default (4bytes->c2 (car lp) (cadr lp) (caddr lp) (cadddr lp)))
	      (lpp    (cddddr lp))
	      (low    (4bytes->c2 (car lpp)(cadr lpp) (caddr lpp) (cadddr lpp)))
	      (lppp   (cddddr lpp))
	      (high   (4bytes->c2 (car lppp)(cadr lppp) (caddr lppp) 
				  (cadddr lppp)))
	      (longueur (+ (length padding) 12 (* 4 (- high low -1)))))
	 (loop (cons `(,mne ,@(list-head (cdr l) longueur)) r)
	       (+ offset longueur 1)
	       (list-tail (cdr l) longueur))))
      ((wide)
       (let ((op-code2 (cadr l)))
	 (if (eqv? op-code2 132)
	     (let*  ((index1  (caddr l))
		     (index2  (cadddr l))
		     (const1  (cadddr (cdr l)))
		     (const2  (cadddr (cddr l))))
	       (loop (cons (list 'wide 132 index1 index2 const1 const2) r)
		     (+ offset 6)
		     (cddddr (cddr l))))
	     (begin
	       (if (not (member (byte->java-mne op-code2)
				'(iload fload aload lload
					dload istore fstore astore 
					lstore dstore ret)))
		   (error "L'instruction wide n'est pas suivie d'une instruction admissible : " op-code2))
	       (let*  ((index1  (caddr l))
		       (index2  (cadddr l))
		       )
		 (loop (cons (list 'wide op-code2 index1 index2) r)
		       (+ offset 4)
		       (cddddr l)))))))
      (else (error "bytes->mne-octets, mnémonique inconnu " mne))))
  (reverse r))))

(define (byte->java-mne indice)
  (car (list-ref instructions-java-codeop indice)))
(define (mne-java->code-op mne)
  (cadr (assoc mne instructions-java-codeop)))
(define (1byte->c2 v)
  (if (> v 127)
      (- v 2^8)
      v))
(define (2bytes->c2 b1 b2)
  (let ((v (+ (* b1 256) b2)))
    (if (> v 2^15)
	(- v 2^16)
	v)))
(define (4bytes->c2 b1 b2 b3 b4)
  (let ((v (+ (* b1 2^24)  (* b2 2^16) (* b3 256) b4)))
    (if (> v 2^31)
	(- v 2^32)
	v)))

(define (java-mne-branchement? mne)
  (member mne '(jsr jsr_w
		    goto goto_w ifeq ifne iflt ifle ifge  ifgt
		    icmpeq icmple icmpme icmplt  
		    icmpge  ifnull ifnonnull ifacmpeq ifacmpne
		    if_icmpeq if_icmpne if_icmplt if_icmpge
		    if_icmpgt if_icmple if_acmpeq if_acmpne
		    )))

(define (java-mne-special? mne)
  (member mne '(lookupswitch tableswitch)))

(define (java-codifie-special mne operandes format ajoute-bits)
  (case mne
    ((tableswitch)  (codifie-operandes-tableswitch operandes format 
						   ajoute-bits))
    ((lookupswitch) (codifie-operandes-lookupswitch operandes format 
						    ajoute-bits))
    (else (error "mne inconnu"))
    ))

(define (codifie-operandes-tableswitch operandes format ajoute-bits)
  (let* ((arg1   (list-ref operandes 1))
	 (arg2   (list-ref operandes 2))
	 (arg3   (list-ref operandes 3))
	 (f1     (list-ref format 1))
	 (f2     (list-ref format 3))
	 (f3     (list-ref format 5))
	 (f4     (list-ref format 7)))
    
    (ajoute-bits (entier->binaire arg1 f1))
    (ajoute-bits (entier->binaire arg2 f2))
    (ajoute-bits (entier->binaire arg3 f3))
    (let loop ((l (car (cddddr operandes))))
      (if (pair? l)
	  (begin
	    (ajoute-bits (entier->binaire (car l) f4))
	    (loop (cdr l))))
      )))

(define (codifie-operandes-lookupswitch operandes format ajoute-bits)
  (let* ((arg1   (list-ref  operandes 1))
	 (arg2   (list-ref  operandes 2))
	 (f1     (list-ref  format 1))
	 (f2     (list-ref  format 3))
	 (f3     (list-ref  format 5)))
    (ajoute-bits (entier->binaire arg1 f1))
    (ajoute-bits (entier->binaire arg2 f2))
    (let loop ((l (cadddr operandes)))
      (if (pair? l)
	  (begin
	    (ajoute-bits (entier->binaire (caar l) f3))
	    (ajoute-bits (entier->binaire (cdar l) f3))
	    (loop (cdr l))))
      )))


;; I :  dir, string
;;      classes, (listof string)
;;
(define (affiche-code-classes dir classes)
  (let loopA ((lf classes))
    (if (pair? lf)
	(begin
	  (display-ln "Traitement fichier " (car lf))
	  (let* ((classe  (read-class dir (car lf)))
		 (methods (list-ref classe 9)))
	    (let loop ((l methods))
	      (if (pair? l)
	      (let ((attributes (cadddr (cdr (car l)))))
		(let loop2 ((l2 attributes))
		  (if (pair? l2)
		      (if (eqv? 'code (car (caddr (car l2))))
			  (let* ((bytecode (list-ref (caddr (car l2)) 4))
				 (insts-addrs  (java-bytes->instruction  bytecode))
				 (insts        (car insts-addrs))
				 (tailleByteCode  (length bytecode))
				 )
			    (display-ln insts) 
			    (loop2 (cdr l2)))
			  (loop2 (cdr l2)))
		      (loop (cdr l)))))
	      (begin (loopA (cdr lf)))))
	    )))))


;; Remplace les déplacements des branchements pour indiquer le nombre
;; d'instructions à franchir plutôt que le nombre d'octets.
;; De plus, ce déplacement est par rapport à la fin de l'instruction
;; et non par rapport au début de l'instruction comme pour le bytecode Java.
;;
;; Entrée : insts :: [(mne arguments)]
;;          addrs :: [entier], adresses relatives des instructions insts
;; Sortie : [(mne arguments)]
;;
(define (brch-octet->brch-inst insts addrs)
  (let loop ((l insts) (la addrs)
	     (i 0)
	     (r '()))
    (if (pair? l)
	(let* ((inst  (car l))
	       (mne   (inst-mne inst)))
	  (if (java-mne-branchement? mne)
	      (let* ((depl-octet (inst-arg1 inst))
		     (cible      (memberi (+ (car la) depl-octet) addrs))
		     ;; déplacement en nombre d'instructions à partir de la suivante.
		     (depl-inst  (- cible i 1)))
		(loop (cdr l) (cdr la) (+ i 1)
		      (cons (list mne depl-inst) r)))
	      (loop (cdr l) (cdr la) (+ i 1) (cons inst r))))
	(reverse r))))

;; =========================================================================
;;
;; Ecriture de classes sous formes binaires.
;;

;; Écrire la classe class dans le fichier.
;;
;; Entrée : class, une structure class
;; Sortie : écriture en binaire de la classe dans le fichier.
;;
(define (ecrire-classe class dir fichier)
  (my-with-output-to-file (string-append dir fichier)
    (lambda () (ecrire-class class))
    ))

;; Entrée : class, structure de classe
;; Sortie : affiche la structure de classe en binaire (c'est un fichier .class)
;;
(define (ecrire-class class)
  (let* ((magic          (list-ref class 0))
	 (minor-version  (list-ref class 1))
	 (major-version  (list-ref class 2))
	 (cst-pool       (list-ref class 3))
	 (cst-pool-cnt   (vector-length cst-pool))

	 (access-flags   (list-ref class 4))
	 (this-class     (list-ref class 5))
	 (super-class    (list-ref class 6))
	 (interfaces       (list-ref class 7))
	 (interfaces-count (length interfaces))
	 (fields           (list-ref class 8))
	 (fields-count     (length fields))
	 (methods          (list-ref class 9))
	 (methods-count    (length methods))
	 (attributes       (list-ref class 10))
	 (attributes-count (length attributes)))
    (ecrire-long magic)
    (ecrire-mot minor-version)
    (ecrire-mot major-version)
    (ecrire-mot cst-pool-cnt)
    (ecrire-constant-pool cst-pool)
    (ecrire-mot access-flags)
    (ecrire-mot this-class)
    (ecrire-mot super-class)
    (ecrire-mot interfaces-count)
    (ecrire-interfaces interfaces)
    (ecrire-mot fields-count)
    (ecrire-fields fields)
    (ecrire-mot methods-count)
    (ecrire-methods methods)
    (ecrire-mot attributes-count)
    (ecrire-attributes attributes)
    ))

(define (write-byte b)  (write-char (integer->char b)))

;; mot, 16 bits
(define (ecrire-mot mot)
  (if (or (< mot 0) (> mot 65535))
      (error "ecrire-mot, Invalid value " mot))
  (let* ((octet1 (quotient mot 256))
	 (octet2 (remainder mot 256)))
    (write-byte octet1)
    (write-byte octet2)
    ))

;; long, 32 bits.
(define (ecrire-long long)
  (if (or (< long 0) (> long 4294967296))
      (error "ecrire-long, value is invalid " long))
  (let* ((mot1 (quotient long 65536))
	 (mot2 (remainder long 65536)))
    (ecrire-mot mot1)
    (ecrire-mot mot2)
    ))
	 
(define (ecrire-constant-pool cst-pool)
  (for-each ecrire-cst (vector->list cst-pool)))

(define (ecrire-cst tag-cst)
  (if (not (member tag-cst '(unusable constante-vide)))
      (let* ((tag  (car tag-cst))
	     (cst  (cadr tag-cst)))
	(write-byte tag)
	(case tag
	  ((7)  (ecrire-constant-class cst))
	  ((9)  (ecrire-constant-fieldref cst))
	  ((10) (ecrire-constant-methodref cst))
	  ((11) (ecrire-constant-interface cst))
	  ((8)  (ecrire-constant-string cst))
	  ((3)  (ecrire-constant-integer cst))
	  ((4)  (ecrire-constant-float cst))
	  ((5)  (ecrire-constant-long cst))
	  ((6)  (ecrire-constant-double cst))
	  ((12) (ecrire-constant-nameandtype cst))
	  ((1)  (ecrire-constant-utf8 cst))
	  (else (error "Tag inconnu" cst))
	  ))
      ))

(define (ecrire-constant-string cst)
  (ecrire-mot cst))

;; Ce n'est pas correct. Ne fonctionne que si la chaîne contient
;; des caractères < 128.
(define (ecrire-constant-utf8 cst)
  (ecrire-mot (car cst))
  (for-each write-char (string->list (cadr cst))))

(define (ecrire-constant-class cst)
  (ecrire-mot cst))

(define (ecrire-constant-fieldref cst)
  (ecrire-mot (car cst))
  (ecrire-mot (cadr cst)))

(define (ecrire-constant-methodref cst)
  (ecrire-mot (car cst))
  (ecrire-mot (cadr cst)))

(define (ecrire-constant-interface cst)
  (ecrire-mot (car cst))
  (ecrire-mot (cadr cst)))

(define (ecrire-constant-integer cst)
  (ecrire-long cst))

(define (ecrire-constant-float cst)
  (ecrire-long cst))

(define (ecrire-constant-long cst)
  (ecrire-long (car cst))
  (ecrire-long (cadr cst)))

(define (ecrire-constant-double cst)
  (ecrire-long (car cst))
  (ecrire-long (cadr cst)))

(define (ecrire-constant-nameandtype cst)
  (ecrire-mot (car cst))
  (ecrire-mot (cadr cst)))  

(define (ecrire-interfaces interfaces)
  (for-each ecrire-mot interfaces))

(define (ecrire-fields fields)
  (define (ecrire-field field)
    (for-each ecrire-mot (list-head field 4))
    (ecrire-attributes (list-ref field 4)))

  (for-each ecrire-field fields))

(define (ecrire-methods methods)
  (define (ecrire-method-info method-info)
    (let* ((access-flags      (list-ref method-info 0))
	   (name-index        (list-ref method-info 1))
	   (descriptor-index  (list-ref method-info 2))
	   (attributes-count  (list-ref method-info 3))
	   (attributes        (list-ref method-info 4)))
      (ecrire-mot access-flags)
      (ecrire-mot name-index)
      (ecrire-mot descriptor-index)
      (ecrire-mot attributes-count)
      (ecrire-attributes attributes)))
  (for-each ecrire-method-info methods))

;; attributes: (listof )
(define (ecrire-attributes attributes)
  (for-each ecrire-attribute-info attributes))

(define (ecrire-attribute-info attribute-info)
  (let*  ((attribute-name-index (car attribute-info))
	  (attribute-length     (cadr attribute-info))
	  (info                 (caddr attribute-info)))
    (ecrire-mot attribute-name-index)
    (ecrire-long attribute-length)
    (ecrire-info info)
    ))

(define (ecrire-info info)
  (let* ((type     (car info))
	 (infoinfo (cdr info)))
    (case type
      ((unknown)       (for-each write-byte infoinfo))
      ((sourceFile)    (ecrire-mot (car infoinfo))) 
      ((constantvalue) (ecrire-mot (car infoinfo)))
      ((code)          (ecrire-attribute-code infoinfo))
      ((exception)     (ecrire-attribute-exception infoinfo))
      (else (error "Info, type inconnu ")))
    ))

(define (ecrire-attribute-exception attribute-exception)
  (let* ((number-of-exceptions  (car attribute-exception))
	 (exception-index-table (cadr attribute-exception)))
    (ecrire-mot number-of-exceptions)
    (for-each 
     (lambda (exception-index) (ecrire-mot exception-index))
     exception-index-table)))

(define (ecrire-attribute-code attribute-code)

  (define (ecrire-code code)
    (for-each write-byte code))

  (define (ecrire-exception-table exception-table)
    (define (ecrire-exception exception)
      (let* ((start-pc   (list-ref exception 0))
	     (end-pc     (list-ref exception 1))
	     (handler-pc (list-ref exception 2))
	     (catch-type (list-ref exception 3)))
	(ecrire-mot start-pc)
	(ecrire-mot end-pc)
	(ecrire-mot handler-pc)
	(ecrire-mot catch-type)
	))
    (for-each ecrire-exception exception-table))

  (let* ((max-stack              (list-ref attribute-code 0))
	 (max-locals             (list-ref attribute-code 1))
	 (code-length            (list-ref attribute-code 2))
	 (code                   (list-ref attribute-code 3))
	 (exception-table-length (list-ref attribute-code 4))
	 (exception-table        (list-ref attribute-code 5))
	 (attributes-count       (list-ref attribute-code 6))
	 (attributes             (list-ref attribute-code 7)))
    (ecrire-mot             max-stack)
    (ecrire-mot             max-locals)
    (ecrire-long            code-length)
    (ecrire-code            code)
    (ecrire-mot             exception-table-length)
    (ecrire-exception-table exception-table)
    (ecrire-mot             attributes-count)
    (ecrire-code-attributes      attributes)
    ))

(define (ecrire-code-attributes attributes)
  (for-each ecrire-code-attribute-info attributes))

(define (ecrire-code-attribute-info attribute-info)
  (let*  ((attribute-name-index     (car attribute-info))
	  (attribute-length         (cadr attribute-info))
	  (info                     (caddr attribute-info)))
    (ecrire-mot  attribute-name-index)
    (ecrire-long attribute-length)
    (ecrire-code-attribute-content-info info)
    ))

(define (ecrire-code-attribute-content-info info)
  (case (car info)
    ((lineNumberTable)
     (ecrire-mot  (cadr info))     
     (for-each (lambda (x) 
		 (ecrire-mot (car x)) ;; start_pc
		 (ecrire-mot (cdr x)) ;; line_number
		 )
	       (caddr info)))
    (else (error "ecrire-code-attribute-content-info, unknow case " info))))
     
;; =========================================================================
;;
;; Ecriture de classes sous forme texte. Utile afin de voir le contenu
;; d'une classe.

;; I: dir
;; Sortie :
;;
(define (dump-class-file dir fclass)
  (txt-write-class (read-class-dir dir fclass)))

;; Entrée : class, class structure.
;; Sortie : write the class structure in text form.
;;
(define (txt-write-class class)
  (display-ln "Beginning writting text description of class file ")
  (let* ((magic          (list-ref class 0))
	 (minor-version  (list-ref class 1))
	 (major-version  (list-ref class 2))
	 (cst-pool       (list-ref class 3))
	 (cst-pool-cnt   (vector-length cst-pool))

	 (access-flags   (list-ref class 4))
	 (this-class     (list-ref class 5))
	 (super-class    (list-ref class 6))
	 (interfaces       (list-ref class 7))
	 (interfaces-count (length interfaces))
	 (fields           (list-ref class 8))
	 (fields-count     (length fields))
	 (methods          (list-ref class 9))
	 (methods-count    (length methods))
	 (attributes       (list-ref class 10))
	 (attributes-count (length attributes)))
    (display-ln "Magic " magic)
    (display-ln "Minor version " minor-version " Major version " major-version)
    (display-ln "Number of pool constants " cst-pool-cnt)
    (txt-ecrire-constant-pool cst-pool)

    (txt-ecrire-access-flags access-flags 'not-method)
    (display-ln "This class " this-class)
    (display-ln "Super class " super-class)
    (display-ln "Number of interfaces " interfaces-count)
    (txt-ecrire-interfaces interfaces)
    (display-ln "Number of fields " fields-count)
    (txt-ecrire-fields fields)
    (display-ln "Number of methods " methods-count)
    (txt-ecrire-methods methods cst-pool)
    (display-ln "Number of attributes " attributes-count)
    (txt-ecrire-attributes attributes)
    ))

(define (txt-ecrire-access-flags access-flags kind)
  (display "Access flags = ")
  (let* ((bit0  (odd? access-flags))
	 (bit1  (odd? (quotient access-flags 2)))
	 (bit2  (odd? (quotient access-flags 4)))
	 (bit3  (odd? (quotient access-flags 8)))
	 (bit4  (odd? (quotient access-flags 16)))
	 (bit5  (odd? (quotient access-flags 32)))
	 (bit6  (odd? (quotient access-flags 64)))
	 (bit7  (odd? (quotient access-flags 128)))
	 (bit8  (odd? (quotient access-flags 256)))
	 (bit9  (odd? (quotient access-flags 512)))
	 (bit10 (odd? (quotient access-flags 1024))))
    (if bit0 (display "public "))
    (if bit1 (display "private "))
    (if bit2 (display "protected "))
    (if bit3 (display "static "))
    (if bit4 (display "final "))
    (if (eqv? kind 'method)
	(if bit5 (display "synchronized "))
	(if bit5 (display "super ")))
    (if bit6 (display "volatile "))
    (if bit7 (display "transient "))
    (if bit8 (display "native "))
    (if bit9 (display "interface "))
    (if bit10 (display "abstract"))
    (newline)
    ))

(define (txt-write-byte b) (display-l b " "))

(define (txt-ecrire-mot mot)  (display-l  mot " "))

(define (txt-ecrire-long long)  (display-l long " "))
	 
(define (txt-ecrire-constant-pool cst-pool)
  (display-ln "------- CONSTANT POOL -------------")
  (for-eachi (lambda (cst i) 
	       (display-l i " ")
	       (txt-ecrire-cst cst))
	     (vector->list cst-pool)
	     0)
  (display-ln "------- END OF CONSTANT POOL ------"))

(define (txt-ecrire-cst tag-cst)
  (if (not (member tag-cst '(unusable constante-vide)))
      (let* ((tag  (car tag-cst))
	     (cst  (cadr tag-cst))
	     )
	(case tag
	  ((1)  (txt-ecrire-constant-utf8 cst))
	  ((3)  (txt-ecrire-constant-integer cst))
	  ((4)  (txt-ecrire-constant-float cst))
	  ((5)  (txt-ecrire-constant-long cst))
	  ((6)  (txt-ecrire-constant-double cst))
	  ((7)  (txt-ecrire-constant-class cst))
	  ((8)  (txt-ecrire-constant-string cst))
	  ((9)  (txt-ecrire-constant-fieldref cst))
	  ((10) (txt-ecrire-constant-methodref cst))
	  ((11) (txt-ecrire-constant-interface cst))
	  ((12) (txt-ecrire-constant-nameandtype cst))
	  (else
	   (error "Tag inconnu" cst))
	  ))
      (display-ln "No constant")
      ))

(define (txt-ecrire-constant-string cst) (display-ln "String index " cst))

;; Ce n'est pas correct. Ne fonctionne que si la chaîne contient
;; des caractères < 256.
(define (txt-ecrire-constant-utf8 cst)
  (display-l "utf8 length = " (car cst))
  (if (not (= (car cst) (string-length (cadr cst))))
      (display-ln "!!! different length for utf8 !!!" 
		  (car cst) (string-length (cadr cst))))
  (display " string = ")
  (for-each write-char (string->list (cadr cst)))
  (newline))

(define (txt-ecrire-constant-class cst)
  (display "class ")
  (txt-ecrire-mot cst)
  (newline))

(define (txt-ecrire-constant-fieldref cst)
  (display-ln "field ref, class index " (car cst) " field index " (cadr cst)))

(define (txt-ecrire-constant-methodref cst)
  (display-ln "method ref, class_index "  (car cst) " name and type " (cadr cst)))

(define (txt-ecrire-constant-interface cst)
  (display "interface ")
  (txt-ecrire-mot (car cst))
  (txt-ecrire-mot (cadr cst))
  (newline))

(define (txt-ecrire-constant-integer cst)
  (display "integer ")
  (txt-ecrire-long cst)
  (newline))

(define (txt-ecrire-constant-float cst)
  (display "float ")
  (txt-ecrire-long cst)
  (newline))

(define (txt-ecrire-constant-long cst)
  (display "long ")
  (txt-ecrire-long (car cst))
  (txt-ecrire-long (cadr cst))
  (newline))

(define (txt-ecrire-constant-double cst)
  (display "double ")
  (txt-ecrire-long (car cst))
  (txt-ecrire-long (cadr cst))
  (newline))

(define (txt-ecrire-constant-nameandtype cst)
  (display "name and type ")
  (txt-ecrire-mot (car cst))
  (txt-ecrire-mot (cadr cst))
  (newline))  

(define (txt-ecrire-interfaces interfaces)
  (display-ln "interfaces ")
  (for-each txt-ecrire-mot interfaces)
  (newline))

(define (txt-ecrire-fields fields)
  (define (txt-ecrire-field field)
    (let* ((access-flags     (list-ref field 0))
	   (name-index       (list-ref field 1))
	   (descriptor-index (list-ref field 2))
	   (attributes-count (list-ref field 3)))
      (txt-ecrire-access-flags access-flags 'field)
      (display-ln " Name index " name-index " descriptor index " descriptor-index)
      (display-ln " Number of attributes "attributes-count)
      (txt-ecrire-attributes (list-ref field 4))))

  (for-eachi (lambda (field i)
	       (display-ln "Field " i)
	       (txt-ecrire-field field))
	     fields
	     0
	     ))

(define (txt-ecrire-methods methods cst-pool)
  (define (txt-ecrire-method-info method-info)
    (let* ((access-flags      (list-ref method-info 0))
	   (name-index        (list-ref method-info 1))
	   (descriptor-index  (list-ref method-info 2))
	   (attributes-count  (list-ref method-info 3))
	   (attributes        (list-ref method-info 4)))
      (txt-ecrire-access-flags access-flags 'method)
      (display-l " Name " name-index " which is " )
      (txt-ecrire-cst (vector-ref cst-pool name-index))
      (display-l " Descriptor "  descriptor-index " wich is ")
      (txt-ecrire-cst (vector-ref cst-pool descriptor-index))
      (display-ln " Number of attributes " attributes-count)
      (txt-ecrire-attributes attributes)
      ))
  (for-eachi (lambda (m i) 
	       (display-ln "Method " i "...") 
	       (txt-ecrire-method-info m)
	       (display-ln "End of method " i)
	       )
	    methods
	    0))


(define (txt-ecrire-attributes attributes)
  (for-each txt-ecrire-attribute-info attributes))

(define (txt-ecrire-attribute-info attribute-info)
  (let*  ((attribute-name-index (car attribute-info))
	  (attribute-length     (cadr attribute-info))
	  (info                 (caddr attribute-info))
	  )
    (display-ln " Name index " attribute-name-index)
    (display-ln " Attribute length " attribute-length)
    (txt-ecrire-info info)))

(define (txt-ecrire-info info)
  (let* ((type     (car info))
	 (infoinfo (cdr info)))
    (case type
      ((unknown)       (for-each txt-write-byte infoinfo))
      ((constantvalue) (display-ln "Constant " (car infoinfo)))
      ((code)          (txt-ecrire-attribute-code infoinfo))
      ((exception)     (txt-ecrire-attribute-exception infoinfo))
      (else (error "Info, type inconnu ")))
    ))

(define (txt-ecrire-attribute-exception attribute-exception)
  (let* ((number-of-exceptions  (car attribute-exception))
	 (exception-index-table (cadr attribute-exception)))
    (display-ln "Number of exceptions " number-of-exceptions)
    (for-each 
     (lambda (exception-index)
       (txt-ecrire-mot exception-index))
     exception-index-table)))

(define (txt-ecrire-attribute-code attribute-code)

  (define (txt-ecrire-code code)
    (display-ln "Code " (car (java-bytes->instruction code))))

  (define (txt-ecrire-exception-table exception-table)
    (define (txt-ecrire-exception exception)
      (let* ((start-pc   (list-ref exception 0))
	     (end-pc     (list-ref exception 1))
	     (handler-pc (list-ref exception 2))
	     (catch-type (list-ref exception 3)))
	(display-ln "start-pc " start-pc)
	(display-ln "end-pc "  end-pc)
	(display-ln "handler-pc " handler-pc)
	(display-ln "catch-type " catch-type)
	))
    (for-each txt-ecrire-exception exception-table))

  (let* ((max-stack              (list-ref attribute-code 0))
	 (max-locals             (list-ref attribute-code 1))
	 (code-length            (list-ref attribute-code 2))
	 (code                   (list-ref attribute-code 3))
	 (exception-table-length (list-ref attribute-code 4))
	 (exception-table        (list-ref attribute-code 5))
	 (attributes-count       (list-ref attribute-code 6))
	 (attributes             (list-ref attribute-code 7)))
    (display-ln "Max stack " max-stack)
    (display-ln "Max locals " max-locals)
    (display-ln "Code length " code-length)
    (txt-ecrire-code code)
    (display-ln "Length of exception table "  exception-table-length)
    (txt-ecrire-exception-table exception-table)
    (display-ln "Number of attributes " attributes-count)
    (txt-ecrire-attributes attributes)
    ))

;; End of writing text description of class file.
;; -----------------------------------------------------------------------------


;; Entrée : inst, une instruction sous la forme (mne arguments)
;; Sortie : (mne format), exemple (add (u 2 u 4 s 5))

(define (java-inst->mne-format inst)
  (let ((mne  (inst-mne inst)))
    (case mne
      ((aaload aastore aconst_null aload_0 aload_1 aload_2 aload_3
	       areturn 
	       arraylength astore_0 astore_1 astore_2 astore_3 athrow
	       baload bastore caload castore d2f d2i d2l dadd daload
	       dastore dcmpg dcmpl dconst_0 dconst_1 ddiv dload_0 dload_1
	       dload_2 dload_3 dmul dneg drem dreturn dstore_0 dstore_1
	       dstore_2 dstore_3 dsub dup dup_x1 dup_x2 dup2 dup2_x1
	       dup2_x2
	       f2d f2i f2l fadd faload fastore fcmpg fcmpl fconst_0
	       fconst_1 fconst_2 fdiv fload_0 fload_1 fload_2 fload_3
	       fmul fneg frem freturn fstore_0 fstore_1 fstore_2 fstore_3
	       fsub i2b i2c i2d i2f i2l i2s iadd iaload iand iastore
	       iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4
	       iconst_5 idiv iload_0 iload_1 iload_2 iload_3 imul ineg
	       ior irem ireturn ishl ishr istore_0 istore_1 istore_2
	       istore_3 isub iushr ixor l2d l2f l2i ladd laload land
	       lastore lcmp lconst_0 lconst_1 ldiv lload_0 lload_1
	       lload_2 lload_3 lmul lneg lor lrem lreturn lshl lshr
	       lstore_0 lstore_1 lstore_2 lstore_3 lsub lushr lxor
	       monitorenter monitorexit nop pop pop2 return saload
	       sastore swap)
       `(,(inst-mne inst) ()))
      ;; Cas un octet non-signé.
      ((aload astore dload dstore fload fstore iload istore ldc lload
	      lstore newarray ret)
       (let ((index (cadr inst)))
	 `(,mne (u ,(nb-bits-u index)))))
      ;; Cas un octet signé.
      ((bipush)
       (let* ((index   (cadr inst))
	      (nb-bits (nb-bits-s index)))
	 `(,mne (s ,nb-bits))))
      ;; Cas deux octets formant un 16 bits non-signé
      ((anewarray checkcast getfield getstatic 
		  instanceof invokespecial invokestatic invokevirtual
		  ldc_w ldc2_w new putfield putstatic)
       (let* ((index (cadr inst)))
	 `(,mne (u ,(nb-bits-u index)))))
      ;; Cas deux octets formant 16 bits signé.
      ((goto if_acmpeq if_acmpne if_icmpeq if_icmpne if_icmplt
	     if_icmpge if_icmpgt if_icmple ifeq ifne iflt ifge
	     ifgt ifle ifnonnull  ifnull jsr sipush)
       (let* ((index    (cadr inst))
	      (nb-bits1 (nb-bits-s index)))
	 `(,mne (s ,nb-bits1))))
	
      ;; Cas 4 octets formant 32 bits signé.
      ((goto_w jsr_w)
       (let* ((index  (cadr inst)))
	 `(,mne (s ,(nb-bits-s index)))))
      ;; Cas premier octet non-signé, second octet signé.
      ((iinc)
       `(,mne (u ,(nb-bits-u (cadr inst)) s ,(nb-bits-s (caddr inst)))))
      ((multianewarray)
       `(,mne (u ,(nb-bits-u (cadr inst)) u ,(nb-bits-u (caddr inst)))))
      ((invokeinterface)
       (let* ((index  (cadr inst))
	      (count  (caddr inst))
	      (zero   (cadddr inst))
	      (nb-bits1 (nb-bits-u index))
	      (nb-bits2 (nb-bits-u count)))
	 `(,mne (u ,nb-bits1 u ,nb-bits2 u 0))))
      ((lookupswitch)
       (let* ((default (caddr inst))
	      (npairs  (cadddr inst))
	      (nb-bits1 (nb-bits-s default))
	      (nb-bits2 (nb-bits-s npairs))
	      (nb-bits3 24)  ; à terminer
	      ) 
	 `(,mne (s ,nb-bits1 s ,nb-bits2 s ,nb-bits3))))
      ((tableswitch)
       (let* ((default   (caddr inst))
	      (low       (cadddr inst))
	      (high      (cadddr (cdr inst)))
	      (nb-bits1  (nb-bits-s default))
	      (nb-bits2  (nb-bits-s low))
	      (nb-bits3  (nb-bits-s high))
	      (nb-bits4  24)) ; à terminer
	 `(,mne (s ,nb-bits1 s ,nb-bits2 s ,nb-bits3 s ,nb-bits4))
	 ))
      ((wide)
       (if (= (cadr inst) 132) ;iinc
	   (let* ((index    (caddr inst))
		  (const    (cadddr inst))
		  (nb-bits1  (nb-bits-u index))
		  (nb-bits2  (nb-bits-s const)))
	     `(,mne (u 8 u ,nb-bits1 s ,nb-bits2)))
	   (let* ((index     (caddr inst))
		  (nb-bits1  (nb-bits-u (cadr inst)))
		  (nb-bits2  (nb-bits-u index)))
	     `(mne (u 8 u ,nb-bits1 u ,nb-bits2))
	     )))
      (else (error "Construit-Formats-Base, mne inconnu " mne)))
    ))


;; Calcule le format de l'instruction qui peut être une macro-instruction.
;; Seulement les types du format de la macro sont utilisés.
;;
;; Entrée : dic-m, dictionnaire de macros [((mne format) seq)]
;; Sortie : un binôme (mne format) de l'instruction
;;
(define (java-inst-macro->mne-format inst dic-m)

  (define (assoc-dic-macro mne)
    (assoc mne (map car dic-m)))

  (define (inst-macro->format inst format)
    (if (not (= (* 2 (length (inst-args inst)))
		(length format)))
	(error "La macro n'a pas le bon nombre d'arguments "
	       "inst= " inst "format macro = " format))
    (list
     (inst-mne inst)
     (list-append
      (map (lambda (arg type)
	     (if (eqv? type 'u)
		 (list 'u (nb-bits-u arg))
		 (list 's (nb-bits-s arg))))
	   (inst-args inst)
	   (filtre symbol? format)))))
  
  (let* ((mne          (inst-mne inst))
	 (macro?       (assoc-dic-macro mne)))
    (if macro?
	(inst-macro->format inst (cadr macro?))
	(java-inst->mne-format inst))
    ))

;; Génère [((mne format) codeop)], mais le format est vide.
;; Utile pour générer un décodeur de 8 bits sans aucune compression.
(define (mnes->mne-format-codeop)
  (map (lambda (mne)
	 (list (list mne '())
	       (entier->binaire
		(cadr (assoc mne instructions-java-codeop)) 8)))
       (map car instructions-java-codeop)))


(define java-formats-base
  '(
    ;; Cas sans paramètres
    (aaload         ())
    (aastore 	    ())
    (aconst_null    ())
    (aload_0	    ())
    (aload_1	    ())
    (aload_2	    ())
    (aload_3	    ())
    (areturn	    ())
    (arraylength    ())
    (astore_0	    ())
    (astore_1	    ())
    (astore_2	    ())
    (astore_3	    ())
    (athrow	    ())
    (baload	    ())
    (bastore	    ())
    (caload	    ())
    (castore	    ())
    (d2f	    ())
    (d2i	    ())
    (d2l	    ())
    (dadd	    ())
    (daload	    ())
    (dastore	    ())
    (dcmpg	    ())
    (dcmpl	    ())
    (dconst_0	    ())
    (dconst_1	    ())
    (ddiv	    ())
    (dload_0	    ())
    (dload_1	    ())
    (dload_2	    ())
    (dload_3	    ())
    (dmul	    ())
    (dneg	    ())
    (drem	    ())
    (dreturn	    ())
    (dstore_0	    ())
    (dstore_1	    ())
    (dstore_2	    ())
    (dstore_3	    ())
    (dsub	    ())
    (dup	    ())
    (dup_x1	    ())
    (dup_x2	    ())
    (dup2	    ())
    (dup2_x1	    ())
    (dup2_x2	    ())
    (f2d	    ())
    (f2i	    ())
    (f2l	    ())
    (fadd	    ())
    (faload	    ())
    (fastore	    ())
    (fcmpg	    ())
    (fcmpl	    ())
    (fconst_0	    ())
    (fconst_1	    ())
    (fconst_2	    ())
    (fdiv	    ())
    (fload_0	    ())
    (fload_1	    ())
    (fload_2	    ())
    (fload_3	    ())
    (fmul	    ())
    (fneg	    ())
    (frem	    ())
    (freturn	    ())
    (fstore_0	    ())
    (fstore_1	    ())
    (fstore_2	    ())
    (fstore_3	    ())
    (fsub	    ())
    (i2b	    ())
    (i2c	    ())
    (i2d	    ())
    (i2f	    ())
    (i2l	    ())
    (i2s	    ())
    (iadd	    ())
    (iaload	    ())
    (iand	    ())
    (iastore	    ())
    (iconst_m1	    ())
    (iconst_0	    ())
    (iconst_1	    ())
    (iconst_2	    ())
    (iconst_3	    ())
    (iconst_4	    ())
    (iconst_5	    ())
    (idiv	    ())
    (iload_0	    ())
    (iload_1	    ())
    (iload_2	    ())
    (iload_3	    ())
    (imul	    ())
    (ineg	    ())
    (ior	    ())
    (irem	    ())
    (ireturn	    ())
    (ishl	    ())
    (ishr	    ())
    (istore_0	    ())
    (istore_1	    ())
    (istore_2	    ())
    (istore_3	    ())
    (isub	    ())
    (iushr	    ())
    (ixor	    ())
    (l2d	    ())
    (l2f	    ())
    (l2i	    ())
    (ladd	    ())
    (laload	    ())
    (land	    ())
    (lastore	    ())
    (lcmp	    ())
    (lconst_0	    ())
    (lconst_1	    ())
    (ldiv	    ())
    (lload_0	    ())
    (lload_1	    ())
    (lload_2	    ())
    (lload_3	    ())
    (lmul	    ())
    (lneg	    ())
    (lor	    ())
    (lrem	    ())
    (lreturn	    ())
    (lshl	    ())
    (lshr	    ())
    (lstore_0	    ())
    (lstore_1	    ())
    (lstore_2	    ())
    (lstore_3	    ())
    (lsub	    ())
    (lushr	    ())
    (lxor	    ())
    (monitorenter   ())
    (monitorexit    ())
    (nop	    ())
    (pop	    ())
    (pop2	    ())
    (return	    ())
    (saload	    ())
    (sastore	    ())
    (swap           ())

    ;; Cas un octet non-signé.
    (aload    (u 8)) 
    (astore   (u 8))
    (dload    (u 8))
    (dstore   (u 8)) 
    (fload    (u 8)) 
    (fstore   (u 8)) 
    (iload    (u 8)) 
    (istore   (u 8)) 
    (ldc      (u 8)) 
    (lload    (u 8))
    (lstore   (u 8)) 
    (newarray (u 8)) 
    (ret      (u 8)) 

    ;; Cas un octet signé
    (bipush  (s 8))

    ;; Cas deux octets formant 16 bits non-signé.
    (anewarray     (u 16))
    (checkcast     (u 16))
    (getfield      (u 16))
    (getstatic     (u 16)) 
    (instanceof    (u 16))
    (invokespecial (u 16))
    (invokestatic  (u 16))
    (invokevirtual (u 16))
    (ldc_w         (u 16))
    (ldc2_w        (u 16))
    (new           (u 16))
    (putfield      (u 16))
    (putstatic     (u 16)) 

    ;; Cas deux octets formant 16 bits signé.
    (goto          (s 16))
    (if_acmpeq     (s 16))
    (if_acmpne     (s 16))
    (if_icmpeq     (s 16))
    (if_icmpne     (s 16))
    (if_icmplt     (s 16))
    (if_icmpge     (s 16))
    (if_icmpgt     (s 16))
    (if_icmple     (s 16))
    (ifeq          (s 16))
    (ifne          (s 16))
    (iflt          (s 16))
    (ifge          (s 16))
    (ifgt          (s 16))
    (ifle          (s 16))
    (ifnonnull     (s 16))
    (ifnull        (s 16))
    (jsr           (s 16))
    (sipush        (s 16))

    ;; Cas 4 octets formant un nombre signé.
    (goto_w        (s 24))
    (jsr_w         (s 24))
     
    ;; Cas premier octet non-signé, second octet signé.
    (iinc          (u 8 s 8))

    ;; Cas complexe.
    
    (multianewarray  (u 16 u 8))
    (invokeinterface (u 16 u 8 u 0))

    ;; (lookupswitch    (s 32 s 32 s 32))
    
    ;; (tableswitch     (s 32 s 32 s 32 s 32))

    ;; l'instruction wide a en fait un autre format initial (u 8 u 8 u 16),
    ;; mais on choisit le supremum. Le premier argument est en fait le
    ;; code opérationnel.
    (wide            (u 8 u 16 u 16))
    ))

;;
;; Toutes les instructions Java en ordre alphabétique
;;
(define instructions-java
  '(aaload aastore aconst_null aload aload_0 aload_1 aload_2 aload_3
	   anewarray areturn arraylength astore astore_0 astore_1 astore_2
	   astore_3 athrow baload bastore bipush breakpoint caload castore
	   checkcast d2f d2i d2l dadd daload dastore dcmpg dcmpl dconst_0
	   dconst_1 ddiv dload dload_0 dload_1 dload_2 dload_3 dmul dneg drem
	   dreturn dstore dstore_0 dstore_1 dstore_2 dstore_3 dsub dup dup2
	   dup2_x1 dup2_x2 dup_x1 dup_x2 f2d f2i f2l fadd faload fastore fcmpg
	   fcmpl fconst_0 fconst_1 fconst_2 fdiv fload fload_0 fload_1 fload_2
	   fload_3 fmul fneg frem freturn fstore fstore_0 fstore_1 fstore_2
	   fstore_3 fsub getfield getstatic goto goto_w i2b i2c i2d i2f i2l i2s
	   iadd iaload iand iastore iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3
	   iconst_4 iconst_5 idiv if_acmpeq if_acmpne if_icmpeq if_icmpge
	   if_icmpgt if_icmple if_icmplt if_icmpne ifeq ifge ifgt ifle iflt ifne
	   ifnonnull ifnull iinc iload iload_0 iload_1 iload_2 iload_3 impdep1
	   impdep2 imul ineg instanceof invokeinterface invokespecial
	   invokenonvirtual invokestatic invokevirtual ior irem ireturn ishl ishr
	   istore istore_0 istore_1 istore_2 istore_3 isub iushr ixor jsr jsr_w
	   l2d l2f l2i ladd laload land lastore lcmp lconst_0 lconst_1 ldc ldc_w
	   ldc2_w ldiv lload lload_0 lload_1 lload_2 lload_3 lmul lneg
	   lookupswitch lor lrem lreturn lshl lshr lstore lstore_0 lstore_1
	   lstore_2 lstore_3 lsub lushr lxor monitorenter monitorexit
	   multianewarray new newarray nop pop pop2 putfield putstatic ret return
	   saload sastore sipush swap tableswitch wide))

;; Il faut que les indices des codes opérationnels soient en séquence
;; croissante en partant de zéro.
;;
(define instructions-java-codeop
'(
  (nop 0)
  (aconst_null 1)
  (iconst_m1 2)
  (iconst_0 3)
  (iconst_1 4)
  (iconst_2 5)
  (iconst_3 6)
  (iconst_4 7)
  (iconst_5 8)
  (lconst_0 9)
  (lconst_1 10)
  (fconst_0 11)
  (fconst_1 12)
  (fconst_2 13)
  (dconst_0 14)
  (dconst_1 15)
  (bipush 16)
  (sipush 17)
  (ldc 18)
  (ldc_w 19)
  (ldc2_w 20)
  (iload 21)
  (lload 22)
  (fload 23)
  (dload 24)
  (aload 25)
  (iload_0 26)
  (iload_1 27)
  (iload_2 28)
  (iload_3 29)
  (lload_0 30)
  (lload_1 31)
  (lload_2 32)
  (lload_3 33)
  (fload_0 34)
  (fload_1 35)
  (fload_2 36)
  (fload_3 37)
  (dload_0 38)
  (dload_1 39)
  (dload_2 40)
  (dload_3 41)
  (aload_0 42)
  (aload_1 43)
  (aload_2 44)
  (aload_3 45)
  (iaload 46)
  (laload 47)
  (faload 48)
  (daload 49)
  (aaload 50)
  (baload 51)
  (caload 52)
  (saload 53)
  (istore 54)
  (lstore 55)
  (fstore 56)
  (dstore 57)
  (astore 58)
  (istore_0 59)
  (istore_1 60)
  (istore_2 61)
  (istore_3 62)
  (lstore_0 63)
  (lstore_1 64)
  (lstore_2 65)
  (lstore_3 66)
  (fstore_0 67)
  (fstore_1 68)
  (fstore_2 69)
  (fstore_3 70)
  (dstore_0 71)
  (dstore_1 72)
  (dstore_2 73)
  (dstore_3 74)
  (astore_0 75)
  (astore_1 76)
  (astore_2 77)
  (astore_3 78)
  (iastore 79)
  (lastore 80)
  (fastore 81)
  (dastore 82)
  (aastore 83)
  (bastore 84)
  (castore 85)
  (sastore 86)
  (pop 87)
  (pop2 88)
  (dup 89)
  (dup_x1 90)
  (dup_x2 91)
  (dup2 92)
  (dup2_x1 93)
  (dup2_x2 94)
  (swap 95)
  (iadd 96)
  (ladd 97)
  (fadd 98)
  (dadd 99)
  (isub 100)
  (lsub 101)
  (fsub 102)
  (dsub 103)
  (imul 104)
  (lmul 105)
  (fmul 106)
  (dmul 107)
  (idiv 108)
  (ldiv 109)
  (fdiv 110)
  (ddiv 111)
  (irem 112)
  (lrem 113)
  (frem 114)
  (drem 115)
  (ineg 116)
  (lneg 117)
  (fneg 118)
  (dneg 119)
  (ishl 120)
  (lshl 121)
  (ishr 122)
  (lshr 123)
  (iushr 124)
  (lushr 125)
  (iand 126)
  (land 127)
  (ior 128)
  (lor 129)
  (ixor 130)
  (lxor 131)
  (iinc 132)
  (i2l 133)
  (i2f 134)
  (i2d 135)
  (l2i 136)
  (l2f 137)
  (l2d 138)
  (f2i 139)
  (f2l 140)
  (f2d 141)
  (d2i 142)
  (d2l 143)
  (d2f 144)
  (i2b 145)
  (i2c 146)
  (i2s 147)
  (lcmp 148)
  (fcmpl 149)
  (fcmpg 150)
  (dcmpl 151)
  (dcmpg 152)
  (ifeq 153)
  (ifne 154)
  (iflt 155)
  (ifge 156)
  (ifgt 157)
  (ifle 158)
  (if_icmpeq 159)
  (if_icmpne 160)
  (if_icmplt 161)
  (if_icmpge 162)
  (if_icmpgt 163)
  (if_icmple 164)
  (if_acmpeq 165)
  (if_acmpne 166)
  (goto 167)
  (jsr 168)
  (ret 169)
  (tableswitch 170)
  (lookupswitch 171)
  (ireturn 172)
  (lreturn 173)
  (freturn 174)
  (dreturn 175)
  (areturn 176)
  (return 177)
  (getstatic 178)
  (putstatic 179)
  (getfield 180)
  (putfield 181)
  (invokevirtual 182)
  (invokespecial 183)
  (invokestatic 184)
  (invokeinterface 185)
  (unused 186)
  (new 187)
  (newarray 188)
  (anewarray 189)
  (arraylength 190)
  (athrow 191)
  (checkcast 192)
  (instanceof 193)
  (monitorenter 194)
  (monitorexit 195)
  (wide 196)
  (multianewarray 197)
  (ifnull 198)
  (ifnonnull 199)
  (goto_w 200)
  (jsr_w 201)
  ;;(breakpoint 202)
  ))

(define (java-codeop->mne codeop)
  (car (list-ref instructions-java-codeop codeop)))
(define (java-class-methods class)
  (list-ref class 9))

;; Retourne une concaténation de tous les codes de
;; toutes les méthodes de la classe.
(define (java-class-methods-code class)
  (list-append
   (map (lambda (method-info)
	  (list-append
	   (map 
	    (lambda (attribute-info)
	      (let* ((info (caddr attribute-info))
		     (type (car info))
		     (infoinfo (cdr info)))
		(if (eqv? type 'code)
		    (list-ref infoinfo 3)
		    '())))
	    (list-ref method-info 4))))
	(java-class-methods class))))

