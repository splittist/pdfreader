;;;; cl-pdf-reader

(in-package #:com.splittist.pdfreader)

;;;# LEXING

(named-readtables:in-readtable syntax)

(defparameter +white-space-characters+ #(#!NUL #!HT #!LF #!FF #!CR #!SP))

(defun white-space-p (octet)
  (find octet +white-space-characters+))

 (defparameter +delimiter-characters+ #(#!( #!) #!< #!> #![ #!] #!{ #!} #!/ #!%))

(defun delimiterp (octet)
  (find octet +delimiter-characters+))

;; "a sequence of consecutive regular characters comprises a single token"
(defun regular-character-p (octet)
  (and (not (white-space-p octet))
       (not (delimiterp octet))))

(defparameter *pdf-stream* nil)

(defun eat-char (expected)
  (let ((char (read-byte *pdf-stream*)))
    (unless (= expected char)
      (error "Wrong character. Expected '~C' and got '~C'" (code-char expected) (code-char char)))))

(defun eat-chars (expected)
  (loop for char across expected do (eat-char char)))

(defun skip-white-space-and-comments (&optional eof-error-p)
  (loop for char = (peek-byte *pdf-stream* t eof-error-p)
	do (cond ((null char)
		  (return :eof))
		 ((= #!% char)
		  (loop for c = (read-byte *pdf-stream* nil)
			until (or (null c)
				  (= #!Newline c)
				  (= #!Return c)
				  (= #!Linefeed c))
			finally (when (= #!Return c)
				  (alexandria:when-let ((octet (peek-byte *pdf-stream* nil nil)))
				    (when (= #!Linefeed octet)
				      (read-byte *pdf-stream*))))))
		 ((not (white-space-p char))
		  (return char))
		 (t
		  (read-byte *pdf-stream*)))))

(defun read-object (&optional (eof-error-p t))
  (skip-white-space-and-comments eof-error-p)
  (let ((char (peek-byte *pdf-stream* nil eof-error-p)))
    (cond ((null char)
	   :eof)
	  ((= 40 char) ; ( stupid emacs
	   (read-literal-string))
	  ((= #!/ char)
	   (read-name))
	  ((= #!< char)
	   (eat-char #!<)
	   (let ((next-char (peek-byte *pdf-stream*)))
	     (if (= #!< next-char)
		 (read-dictionary-or-stream)
		 (read-hexadecimal-string t))))
	  ((= #![ char)
	   (read-array))
	  ((or (digit-octet-p char)
	       (= #!+ char)
	       (= #!- char)
	       (= #!. char))
	   (read-number-or-keyword))
	  (t
	   (read-keyword)))))

(defun read-hex-digit (&aux (octet (read-byte *pdf-stream*)))
  (or (digit-octet-p octet 16) (error "Invalid hexadecimal octet: ~D" octet)))

;; name cannot include NUL, but we are a reader so we'll be lenient
(defun read-name ()
  (eat-char #!/)
  (let ((octets (serapeum:vect)))
    (loop for octet = (peek-byte *pdf-stream* nil nil :eof)
	  until (eq :eof octet)
	  until (= #!NUL octet)
	  while (regular-character-p octet)
	  if (= #!# octet)
	    do (eat-char #!#)
	       (vector-push-extend (+ (ash (read-hex-digit) 4) (read-hex-digit)) octets)
	  else do
	    (vector-push-extend (read-byte *pdf-stream*) octets))
    (intern-name octets)))

#|
(defparameter *name-test*
  '(#"/Name1" #"/ASomewhatLongerName" #"/A;Name_With-Various***Characters"
    #"/1.2" #"/$$" #"/@pattern" #"/.notdef" #"/Lime#20Green"
    #"/paired#28#29parentheses" #"/The_Key_of_F#23_Minor" #"/A#42"))

(dolist (name *name-test*)
  (let ((result (with-input-from-octet-vector (*pdf-stream* name) (read-name))))
    (write result :escpae nil)
     (print result)))
|#

(defun read-literal-string ()
  (eat-char 40)	; ( stupid emacs
  (let ((out (serapeum:vect)))
    (labels ((write-octet (octet)
	       (vector-push-extend octet out)))
      (loop with escape = nil
	    with octal = nil
	    with octal-count = 0
	    with parens = 0
	    for char = (read-byte *pdf-stream*)
	    if (= #!\ char)
	      do (cond (escape
			(write-octet 92) ; \ stupid emacs
			(setf escape nil))
		       (t
			(setf escape t)))
	    else if (and octal
			 (digit-octet-p char 8))
		   do (cond ((= 2 octal-count)
			     (write-octet (logand (+ (ash octal 3)
						     (digit-octet-p char 8))
						  #o377))
			     (setf octal nil
				   octal-count 0))
			    (t
			     (setf octal (+ (ash octal 3) (digit-octet-p char 8)))
			     (incf octal-count)))
	    else do (cond (escape
			   (case char
			     ((#!Newline #!Return #!Linefeed)
			      (when (and (= #!Return char)
					 (eql #!Linefeed
					      (peek-byte *pdf-stream* nil nil)))
				(read-byte *pdf-stream*)))
			     (#!n
			      (write-octet #!LF))
			     (#!r
			      (write-octet #!CR))
			     (#!t
			      (write-octet #!HT))
			     (#!b
			      (write-octet #!BS))
			     (#!f
			      (write-octet #!FF))
			     (40	; ( stupid emacs
			      (write-octet 40))
			     (41	; ) stupid emacs
			      (write-octet 41))
			     (t
			      (alexandria:if-let ((number (digit-octet-p char 8)))
				(setf octal number
				      octal-count 1)
				(write-octet char))))
			   (setf escape nil))
			  (t
			   (when octal
			     (write-octet (logand octal #o377))
			     (setf octal nil
				   octal-count 0))
			   (case char
			     (40 (incf parens)) ; ( stupid emacs
			     (41 (when (zerop parens) (loop-finish))
			      (decf parens))) ; ) stupid emacs
			   (write-octet char)))
	    finally (when octal
		      (write-octet (logand octal #o377))))
      (make-pdf-string out))))

(defun read-hexadecimal-string (&optional <-eaten)
  (unless <-eaten
    (eat-char #!<))
  (let ((octets (serapeum:vect)))
    (loop with prev = nil
	  for char = (read-byte *pdf-stream*)
	  until (= #!> char)
	  unless (white-space-p char)
	    do (cond ((not (digit-octet-p char 16))
		      (error "Wrong thing to be a hexdigit: ~C" (code-char char)))
		     (prev
		      (vector-push-extend (+ (ash prev 4)
					     (digit-octet-p char 16))
					  octets)
		      (setf prev nil))
		     (t
		      (setf prev (digit-octet-p char 16))))
	  finally (when prev
		    (vector-push-extend (ash prev 4) octets)))
    (make-pdf-string octets)))

(defun read-keyword ()
  (let ((octets (make-array 0 :element-type 'octet :adjustable t :fill-pointer 0)))
    (loop for char = (peek-byte *pdf-stream* nil nil)
	  until (null char)
	  while (regular-character-p char)
	  do (read-byte *pdf-stream*)
	     (vector-push-extend char octets))
    (cond ((octets= #"true" octets)
	   +true+)
	  ((octets= #"false" octets)
	   +false+)
	  ((octets= #"null" octets)
	   +null+)
	  (t
	   (make-pdf-keyword octets)))))

(defun read-number-or-keyword ()
  (let ((octets (serapeum:vect)))
    (loop for char = (peek-byte *pdf-stream* nil nil)
	  while char
	  while (regular-character-p char)
	  do (read-byte *pdf-stream*)
	     (vector-push-extend char octets))
    (let ((start (if (find (aref octets 0) #"+-") 1 0)))
      (if (and (<= (count #!. (subseq octets start)) 1)
	       (every 'digit-octet-p (remove #!. (subseq octets start))))
	  (make-pdf-number (read-from-string (octets-latin1 octets)))
	  (with-input-from-octet-vector (*pdf-stream* octets)
	    (read-keyword))))))

(defun read-array ()
  (eat-char #![)
  (let ((stack '()))
    (loop (skip-white-space-and-comments)
	  (serapeum:case-let (char (peek-byte *pdf-stream*))
	    (#!]
	     (eat-char #!])
	     (return))
	    (#!R
	     (eat-char #!R)
	     (let ((generation (get-integer (pop stack)))
		   (object (get-integer (pop stack))))
	       (push (fetch-indirect-object object generation) stack)))
	    (t
	     (push (read-object) stack))))
    (make-pdf-array (nreverse stack))))

(defun read-dictionary-properties ()
  (eat-char #!<)
  (let ((plist '()))
    (loop (skip-white-space-and-comments)
	  (serapeum:case-let (char (peek-byte *pdf-stream*))
	    (#!>
	     (eat-chars #">>")
	     (return))
	    (#!R
	     (eat-char #!R)
	     (let ((generation (get-integer (pop plist)))
		   (object (get-integer (pop plist))))
	       (push (fetch-indirect-object object generation) plist)))
	    (t
	     (push (read-object) plist))))
    (alexandria:plist-alist (nreverse plist))))

(defun read-dictionary-or-stream ()
  (let ((properties (read-dictionary-properties)))
    (skip-white-space-and-comments)
    (let ((char (peek-byte *pdf-stream* nil nil)))
      (cond ((eql #!s char)
	     (eat-chars #"stream")
	     (when (= #!Return (peek-byte *pdf-stream*))
	       (eat-char #!Return))
	     (eat-char #!Linefeed)
	     (read-pdf-stream properties))
	    (t
	     (make-pdf-dictionary properties))))))

(defun read-dictionary ()
  (make-pdf-dictionary (read-dictionary-properties)))

(defun read-pdf-stream (properties)
  (let ((length-object
	  (serapeum:assocdr #"Length" properties
			    :key 'pdf-object-value :test 'octets=)))
    (when (typep length-object 'indirect-object)
      (let ((position (file-position *pdf-stream*)))
	(setf length-object (content (load-indirect-object length-object)))
	(file-position *pdf-stream* position)))
    (let* ((length (get-integer length-object))
	   (content (make-array length :element-type 'octet))
	   (read (read-sequence content *pdf-stream*)))
      (when (< read length)
	(error "Unexpected end of file in content stream"))
      (skip-white-space-and-comments)
      (eat-chars #"endstream")
      (make-pdf-stream properties content))))
      
