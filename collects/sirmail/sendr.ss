
(module sendr mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "mred-sig.ss" "mred"))

  (require (lib "list.ss"))

  (require "sirmails.ss")

  (require (lib "imap-sig.ss" "net")
	   (lib "smtp-sig.ss" "net")
	   (lib "head-sig.ss" "net")
	   (lib "base64-sig.ss" "net"))

  (require (lib "hierlist-sig.ss" "hierlist"))

  (provide send@)
  (define send@
    (unit/sig sirmail:send^
      (import (exit-sirmail)
	      sirmail:utils^
	      sirmail:options^
	      mred^
	      net:imap^
	      net:smtp^
	      net:head^
	      net:base64^
	      hierlist^
	      (install-text-functions)
	      (install-emacs-bindings))

              
      ;; queue-directory : string
      ;; the directory where queue'd files are stored (created at this point)
      (define queue-directory 
        (let ([dir (build-path (find-system-path 'pref-dir) 
                               (if (eq? 'unix (system-type)) 
                                   ".sirmail-queue"
                                   "SirMail Queue"))])
          (unless (directory-exists? dir)
            (make-directory dir))
          dir))
      
      
      (define FRAME-WIDTH 560)
      (define FRAME-HEIGHT 600)
      (let-values ([(display-width display-height) (get-display-size)])
	(set! FRAME-HEIGHT (min display-height FRAME-HEIGHT))
	(set! FRAME-WIDTH (min display-width FRAME-WIDTH)))

      (define FORWARD-LIST-HEIGHT 50)

      (define SMTP-SERVER (car SMTP-SERVERS))

      (define return-bitmap
	(with-handlers ([void (lambda () #f)])
	  (let ([bm (make-object bitmap% 
				 (build-path 
				  (collection-path "icons")
				  "return.xbm"))])
	    (and (send bm ok?) bm))))

      (define send-icon (make-object bitmap% (build-path (collection-path "sirmail")
							 "stamp.bmp")))
      (define send-icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
							      "stamp-mask.xbm")))
      (unless (and (send send-icon ok?)
		   (send send-icon-mask ok?))
	(set! send-icon #f))

      (define SEPARATOR (make-string 80 #\=))

      ;; Returns a list of <full>-<address> pairs
      (define (resolve-alias addr)
	(cond
	 [(assoc addr ALIASES)
	  => (lambda (m)
	       (let ([resolve
		      (lambda (n)
			(let ([l (sm-extract-addresses n)])
			  (unless (> (length l) 0)
			    (error 'resolve-alias "alias is not an address: ~a" n))
			  l))])
		 (if (list? (cadr m))
		     (apply append (map resolve (cadr m)))
		     (resolve (cadr m)))))]
	 [DEFAULT-DOMAIN (let ([addr (format "~a@~a" addr DEFAULT-DOMAIN)])
			   (list (cons addr addr)))]
	 [else (list (cons addr addr))]))

      ;; Returns a list of <full>-<address> pairs
      (define (sm-extract-addresses s)
	(let ([addrs (extract-addresses s 'all)])
	  (apply
	   append
	   (map
	    (lambda (a)
	      (let ([name (car a)]
		    [address (cadr a)]
		    [full (caddr a)])
		(if (and (string=? address full)
			 (not (regexp-match "@" full)))
		    (resolve-alias full)
		    (list (cons full address)))))
	    addrs))))
      
      (define (remove-fields l h)
	(if (null? l)
	    h
	    (remove-fields (cdr l) (remove-field (car l) h))))

      (define-struct enclosure (name      ; identifies enclosure in the GUI
				subheader ; header for enclosure
				data))    ; enclosure data (already encoded)

      ;; Create a message with enclosures.
      ;;  `header' is a message header created with the head.ss library
      ;;  `body' is a string with CRLF-delimmited lines
      ;;  `enclosures' is a list of `enclosure' structs
      (define (enclose header body enclosures)
	(if (null? enclosures)
	    (values header body)
	    (let ([boundary
		   ;; Generate something that isn't there:
		   (let loop ()
		     (let* ([b (format "---~a~a~a-----" (random 10000) (random 10000) (random 10000))]
			    [m (regexp b)])
		       (if (or (regexp-match-positions m body)
			       (ormap
				(lambda (enc)
				  (or (regexp-match-positions m (enclosure-subheader enc))
				      (regexp-match-positions m (enclosure-data enc))))
				enclosures))
			   (loop)
			   b)))])
	      (let ([mime-header (insert-field
				  "MIME-Version"
				  "1.0"
				  (insert-field
				   "Content-Type"
				   (data-lines->data
				    (list
				     "multipart/mixed;"
				     (format "boundary=~s"
					     boundary)))
				   empty-header))])
		(values (append-headers header mime-header)
			(string-append
			 (apply
			  string-append
			  "This is a multi-part message in MIME format."
			  crlf "--" boundary crlf
			  (insert-field
			   "Content-Type"
			   "text/plain; charset=us-ascii"
			   (insert-field
			    "Content-Transfer-Encoding"
			    "7bit"
			    empty-header))
			  body
			  (map
			   (lambda (enc)
			     (string-append
			      crlf "--" boundary crlf
			      (enclosure-subheader enc)
			      (enclosure-data enc)))
			   enclosures))
			 crlf "--" boundary "--" crlf))))))

      ;; new-mailer : ... -> frame[with send-message method]
      (define (new-mailer file to cc subject other-headers body enclosures)
	(define f% (class100 frame% args
		     (inherit get-menu-bar set-icon)
                     (public 
                       [send-message 
                        (lambda () (send-msg))])
                     (override
		       [can-close?
			(lambda ()
			  (and (send (get-menu-bar) is-enabled?)
			       (or (not (send e is-modified?))
				   (eq? 'yes
					(message-box
					 "Warning"
					 "The message is not saved or sent. Close anyway?"
					 this
					 '(yes-no))))))]
		       [on-close
			(lambda () (exit-sirmail))])
		     (sequence
		       (apply super-init args)
		       (when send-icon
			 (set-icon send-icon send-icon-mask)))))
	(define f (make-object f% "Send Mail" #f FRAME-WIDTH FRAME-HEIGHT))
	(define mb (make-object menu-bar% f))
	(define file-menu (make-object menu% "File" mb))
	(define edit-menu (make-object menu% "Edit" mb))
	(define button-pane (make-object horizontal-pane% f))
	(define title-message (make-object message% "Compose message" button-pane)) 
	(define button-pane-spacer (make-object vertical-pane%  button-pane))
	(define cancel-button
	  (make-object button% "Stop" button-pane
		       (lambda (b e) (cancel-button-todo))))
	(define cancel-button-todo void)
	(define c (make-object editor-canvas% f))
	(define e (make-object text%))
	(define enclosure-list (make-object hierarchical-list% f))

	(define (enable on? refocus cancel-proc)
	  (let ([w (send f get-focus-window)])
	    (set! cancel-button-todo cancel-proc)
	    (send mb enable on?)
	    (send c enable on?)
	    (send cancel-button enable (not on?))
	    (when (and on? refocus)
	      (send refocus focus))
	    w))

	(define (clean-filename name)
	  (regexp-replace* "[ /:\\\"'`?*%<>$|]" name "_"))

	(define (parse-server-name s)
	  (let ([m (regexp-match "^([^:]*):([^:]*)$" s)])
	    (if (and m (string->number (caddr m)))
		(values (cadr m) (string->number (caddr m)))
		(values s 25))))

	(define-values (smtp-server-to-use smtp-port-to-use)
	  (parse-server-name SMTP-SERVER))

	(define (send-msg)
	  (let ([t (lf->crlf (send e get-text))]
		[re (regexp (format "~a~a" SEPARATOR crlf))])
	    (let ([m (regexp-match-positions re t)])
	      (if m
		  (let ([header (string-append (substring t 0 (caar m)) empty-header)]
			[body (substring t (cdar m) (string-length t))])
		    (validate-header header)
		    (let* ([to* (sm-extract-addresses (extract-field "To" header))]
			   [to (map car to*)]
			   [cc* (sm-extract-addresses (extract-field "CC" header))]
			   [cc (map car cc*)]
			   [bcc* (sm-extract-addresses (extract-field "BCC" header))]
			   [bcc (map car bcc*)]
			   [from (let ([l (extract-addresses MAIL-FROM 'full)])
				   (unless (= 1 (length l))
				     (error 'send "bad mail-from configuration: ~a" MAIL-FROM))
				   (car l))]
			   [subject (extract-field "Subject" header)]
			   [prop-header (remove-fields '("To" "CC" "BCC" "Subject") header)]
			   [std-header (standard-message-header from to cc bcc subject)]
			   [new-header (append-headers std-header prop-header)]
			   [tos (map cdr (append to* cc* bcc*))]
			   [enclosures (map (lambda (i) (send i user-data)) 
					    (send enclosure-list get-items))])
		      (let-values ([(new-header body) (enclose new-header body enclosures)])
			(when SAVE-SENT
			  (let* ([chop (lambda (s)
					 (let ([l (string-length s)])
					   (clean-filename (substring s 0 (min l 10)))))]
				 [to (if (null? tos) "noone" (chop (car tos)))]
				 [subj (if subject (chop subject) "nosubj")])
			    (let loop ([n 1])
			      (let ([fn (build-path SAVE-SENT (format "~a_~a_~a" to subj n))])
				(if (file-exists? fn)
				    (loop (add1 n))
				    (with-output-to-file fn
				      (lambda ()
					(display (crlf->lf header))
					(display (crlf->lf body)))))))))
			(as-background
			 enable
			 (lambda (break-bad break-ok)
			   (send f set-status-text "Sending mail...")
			   (with-handlers ([void (lambda (x)
						   (send f set-status-text "")
						   (raise x))])
			     (break-ok)
			     (smtp-sending-end-of-message break-bad)
			     (smtp-send-message smtp-server-to-use
						from
						tos
						new-header
						(split-crlf body)
						smtp-port-to-use)))
			 (lambda ()
			   (let loop ()
			     (when (eq? (message-box "Save?" "Save message before killing?" #f '(yes-no))
					'yes)
			       (let ([f (put-file)])
				 (if f
				     (send e save-file f 'text)
				     (loop)))))))))
		    (send f on-close)
		    (send f show #f))
		  (message-box
		   "Error"
		   (format "Lost \"~a\" separator" SEPARATOR))))))

        ;; enq-msg : -> void
        ;; enqueues a message for a later send
	(define (enq-msg)
          (let ([filename (get-fresh-queue-filename)])
            (send e save-file filename 'text))
          
          (when (send f can-close?)
            (send f on-close)
            (send f show #f)))
        
        ;; get-fresh-queue-filename : -> string
        (define (get-fresh-queue-filename)
          (build-path queue-directory 
                      (format "enq~a" (+ 1 (length (directory-list queue-directory))))))
        
        (send button-pane stretchable-height #f)
	(send cancel-button enable #f)

	(send enclosure-list stretchable-height #f)
	(send enclosure-list min-height FORWARD-LIST-HEIGHT)
	(when (null? enclosures)
	  (send f delete-child enclosure-list))
	(for-each
	 (lambda (enc)
	   (let ([i (send enclosure-list new-item)])
	     (send (send i get-editor) insert (enclosure-name enc))
	     (send i user-data enc)))
	 enclosures)
	
	(make-object menu-item% "Save" file-menu 
		     (lambda (i ev) (send e save-file #f 'text)))
	(make-object menu-item% "Send" file-menu (lambda (i ev) (send-msg)))
        (make-object menu-item% "Enqueue message" file-menu (lambda (i ev) (enq-msg)))
	(make-object separator-menu-item% file-menu)
	(make-object menu-item% "Add Enclosure..." file-menu
		     (lambda (i env)
		       (let ([file (get-file)])
			 (when file
			   (let* ([types '("application/postscript"
					   "text/plain"
					   "application/octet-stream")]
				  [type (get-choices-from-user
					 "Content Type"
					 "Type of enclosure:"
					 types
					 f
					 '(0))])
			     (when type
			       (let* ([encodings '("7bit"
						   "base64")]
				      [encoding (get-choices-from-user
						 "Content Encoding"
						 "Encoding for enclosure:"
						 encodings
						 f
						 '(0))])
				 (when encoding
				   (let ([type (list-ref types (car type))]
					 [encoding (list-ref encodings (car encoding))])
				     (let ([i (send enclosure-list new-item)]
					   [enc (make-enclosure
						 file
						 (insert-field
						  "Content-Type" 
						  (data-lines->data
						   (list
						    (string-append type ";")
						    (format "name=~s" (clean-filename
								       (with-handlers ([void (lambda (x) "unknown")])
									 (let-values ([(base name dir?) (split-path file)])
									   name))))))
						  (insert-field
						   "Content-Transfer-Encoding" encoding
						   empty-header))
						 (let ([content (with-input-from-file file
								  (lambda ()
								    (read-string (file-size file))))])
						   (case (string->symbol encoding)
						     [(base64) (base64-encode content)]
						     [(7bit) (lf->crlf content)])))])
				       (send (send i get-editor) insert (enclosure-name enc))
				       (send i user-data enc)
				       (unless (memq enclosure-list (send f get-children))
					 (send f add-child enclosure-list))))))))))))
	(make-object separator-menu-item% file-menu)
	(when SMTP-SERVERS
	  (let ([m (make-object menu% "SMTP Server" file-menu)])
	    (for-each
	     (lambda (s)
	       (let ([i (make-object checkable-menu-item% s m
				     (lambda (i e)
				       (for-each (lambda (i) (send i check #f)) (send m get-items))
				       (set! SMTP-SERVER s)
				       (set!-values (smtp-server-to-use smtp-port-to-use)
						    (parse-server-name s))
				       (send i check #t)))])
		 (when (string=? s SMTP-SERVER)
		   (send i check #t))))
	     SMTP-SERVERS))
	  (make-object separator-menu-item% file-menu))
	(make-object menu-item% "Close" file-menu
		     (lambda (i e) 
		       (when (send f can-close?)
			 (send f on-close)
			 (send f show #f)))
		     (if (eq? (system-type) 'windows) #f #\W))
	(append-editor-operation-menu-items edit-menu #t)
					; Strip menu key bindings
	(for-each
	 (lambda (i)
	   (when (is-a? i selectable-menu-item<%>)
	     (send i set-shortcut #f)))
	 (send edit-menu get-items))

	(let ([km (send e get-keymap)])
	  (send km add-function "reflow-paragraph"
		(lambda (e ev) (reflow-paragraph 
				e
				(add1 (send e find-string SEPARATOR
					    'forward 0 'eof #f)))))
	  (send km map-function ":m:q" "reflow-paragraph")
	  (send km map-function 
		(string-append ":a:" (string (integer->char 207)))
		"reflow-paragraph")

	  (install-text-functions km)
	  (install-emacs-bindings km)
	  
	  (send km add-function "send-message"
		(lambda (w e) (send-msg)))
	  (send km map-function ":m:return" "send-message")
	  (send km map-function ":a:return" "send-message"))

	(make-fixed-width c e #t return-bitmap)
	(send e set-paste-text-only #t)
	(send e set-max-undo-history 5000) ;; Many undos!
	(send c set-editor e)

	(if file
					; Resume a composition...
	    (send e load-file file)
					; Build message skeleton
	    (begin
	      (send e insert "To: ")
	      (send e insert (crlf->lf to))
	      (send e insert #\newline)
	      (unless (string=? cc "")
		(send e insert "CC: ")
		(send e insert (crlf->lf cc))
		(send e insert #\newline))
	      (send e insert "Subject: ")
	      (send e insert (crlf->lf subject))
	      (send e insert #\newline)
	      (send e insert (crlf->lf other-headers))
	      (send e insert "X-Mailer: SirMail under MrEd ")
	      (send e insert (version))
	      (send e insert " (")
	      (send e insert (system-library-subpath))
	      (send e insert ")")
	      (send e insert #\newline)
	      (send e insert SEPARATOR)
	      (send e insert #\newline)
	      (let ([message-start (send e last-position)])
		(send e insert (crlf->lf body))
		(if (string=? to "")
		    (send e set-position (send e paragraph-end-position 0))
		    (send e set-position message-start)))))

	(send e set-modified #f)
	(send e scroll-to-position 0)
	(send c focus)

	(send f create-status-line)

	(send f show #t)
        f)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Meta-Q Reflowing                                      ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define reflow-wordbreak-map
	(make-object editor-wordbreak-map%))
      (send reflow-wordbreak-map set-map #\- '(line))
      (define (reflow-paragraph edit start-min)
	(let ([wbm (send edit get-wordbreak-map)])
	  (dynamic-wind
	      (lambda ()
		(send edit set-wordbreak-map reflow-wordbreak-map)
		(send edit begin-edit-sequence))
	      (lambda ()
		(let ([p (max start-min (send edit get-start-position))]
		      [min-line (send edit position-paragraph start-min)])
		  (let loop ([start-l (send edit position-paragraph p)])
		    (if (or (<= start-l min-line)
			    (= (send edit paragraph-start-position start-l)
			       (add1 (send edit paragraph-start-position (sub1 start-l)))))
			(let loop ([end-l start-l])
			  (if (or (= end-l (send edit last-paragraph))
				  (= (send edit paragraph-end-position end-l)
				     (sub1 (send edit paragraph-end-position (add1 end-l)))))
			      (let ([orig-start (send edit paragraph-start-position start-l)]
				    [end (send edit paragraph-end-position end-l)]
				    [second-line-prefix
				     (if (= start-l end-l)
					 ""
					 (let ([p (send edit paragraph-start-position (add1 start-l))])
					   (let loop ([pe p])
					     (case (send edit get-character pe)
					       [(#\space #\tab #\>) (loop (add1 pe))]
					       [else (send edit get-text p pe)]))))])
				(let ([start ; skip spaces on first line (if there's a non-space):
				       (let ([start-end (send edit paragraph-end-position start-l)])
					 (let loop ([start orig-start])
					   (cond
					    [(= start-end start) orig-start]
					    [(memq (send edit get-character start) '(#\space #\tab))
					     (loop (add1 start))]
					    [else start])))])
					; Remove all line breaks and double-spaces
					; spaces
				  (let loop ([start start]
					     [end end]
					     [l (list (string-append (string #\newline)
								     second-line-prefix)
						      (string #\newline)
						      (string #\tab)
						      (string #\space #\space))])
				    (let ([p (send edit find-string (car l)
						   'forward start end)]
					  [line-break (string-append (string #\newline)
								     second-line-prefix)]
					  [slp-len (string-length second-line-prefix)])
				      (if (or p (pair? (cdr l)))
					  (if p
					      (let ([len (string-length (car l))])
						(send edit insert " " p (+ p len))
						(loop start (- end len -1) l))
					      (loop start end (cdr l)))
					; Insert good line breaks
					  (let loop ([start start]
						     [len (- start orig-start)]
					; First, remove ending space
						     [end (if (or (= end start)
								  (not (char=?
									#\space
									(send edit get-character 
									      (sub1 end)))))
							      end
							      (begin
								(send edit delete (sub1 end) end)
								(sub1 end)))])
					    (unless (>= start end)
					      (let ([ebox (box start)])
						(send edit find-wordbreak #f ebox 'line)
						(let* ([p (unbox ebox)]
						       [wlen (- p start)])
						  (cond
						   [(or (zero? len) (< (+ len wlen) 72))
						    (loop p (+ len wlen) end)]
						   [(char=? #\space (send edit get-character start))
						    (send edit insert line-break start (add1 start))
						    (loop (+ p slp-len) (+ wlen -1 slp-len) 
							  (+ slp-len end))]
						   [else
						    (send edit insert line-break start)
						    (loop (+ p 1 slp-len) (+ wlen slp-len)
							  (+ end 1 slp-len))]))))))))))
			      (loop (add1 end-l))))
			(loop (sub1 start-l))))))
	      (lambda () 
		(send edit end-edit-sequence)
		(send edit set-wordbreak-map wbm)))
	  #t)))))
