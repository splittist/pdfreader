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
			     (write-octet (logand (+ (ash octal 3) (digit-octet-p char 8)) #o377))
			     (setf octal nil
				   octal-count 0))
			    (t
			     (setf octal (+ (ash octal 3) (digit-octet-p char 8)))
			     (incf octal-count)))
	    else do (cond (escape
			   (case char
			     ((#!Newline #!Return #!Linefeed)
			      (when (and (= #!Return char)
					 (eql #!Linefeed (peek-byte *pdf-stream* nil nil)))
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
			     (41 (when (zerop parens) (loop-finish)) (decf parens))) ; ) stupid emacs
			   (write-octet char)))
	    finally (when octal
		      (write-octet (logand octal #o377))))
      (make-instance 'pdf-string :value out))))

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
		      (vector-push-extend (+ (ash prev 4) (digit-octet-p char 16)) octets)
		      (setf prev nil))
		     (t
		      (setf prev (digit-octet-p char 16))))
	  finally (when prev
		    (vector-push-extend (ash prev 4) octets)))
    (make-instance 'pdf-string :value octets)))

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
	   (make-instance 'pdf-keyword :value octets)))))

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
	  (make-instance 'pdf-number :value (read-from-string (octets-latin1 octets)))
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
    (make-instance 'pdf-array :value (nreverse stack))))

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
	     (make-instance 'pdf-dictionary :value properties))))))

(defun read-dictionary ()
  (make-instance 'pdf-dictionary :value (read-dictionary-properties)))

(defun read-pdf-stream (properties)
  (let ((length-object (serapeum:assocdr #"Length" properties :key 'pdf-object-value :test 'octets=)))
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
      (make-instance 'pdf-stream :value properties :content content))))
      
(defun read-indirect-object-content (position)
  (file-position *pdf-stream* position)
  (let* ((object-number (get-integer (read-object)))
	 (generation-number (get-integer (read-object))))
    (declare (ignorable object-number generation-number))
    (skip-white-space-and-comments)
    (eat-chars #"obj")
    (let ((object (read-object)))
      (skip-white-space-and-comments)
      ;; lenient on missing "endobj" per cl-pdf-parser
      (when (= #!e (peek-byte *pdf-stream*))
	(eat-chars #"endobj"))
      object)))

(defun read-indirect-object-content-from-stream (stream-number stream-index)
  (print stream-number)
  (print stream-index) ;; DEBUG
  (let* ((object-stream (ensure-object (read-indirect-object stream-number 0)))
	 (debug (print object-stream)) ;; DEBUG
	 (first (get-integer (get-dict #"First" object-stream)))
	 (n (get-integer (get-dict #"N" object-stream)))
	 (*pdf-stream* (get-stream object-stream)))
    (file-position *pdf-stream* 0)
    (let ((pairs (loop repeat N collecting (cons (get-integer (read-object))
						 (get-integer (read-object))))))
      (print pairs) ;; DEBUG
      (file-position *pdf-stream* (+ first (serapeum:assocdr stream-index pairs))))
      (read-object)))

(defun read-object-stream (object-stream)
  (let* ((first (get-integer (get-dict #"First" object-stream)))
	 #+(or)(n (get-integer (get-dict #"First" object-stream)))
	 (*pdf-stream* (get-stream object-stream)))
    (file-position *pdf-stream* first)
    (read-object)))

(defun read-object-stream* (object-stream)
  (let* ((first (get-integer (get-dict #"First" object-stream)))
	 (n (get-integer (get-dict #"N" object-stream)))
	 (*pdf-stream* (get-stream object-stream)))
    (file-position *pdf-stream* 0)
    (let ((pairs (loop repeat n collecting (cons (get-integer (read-object))
						 (get-integer (read-object))))))
      (file-position *pdf-stream* first)
      (loop for (number . offset) in pairs
	    for index from 0
	    for object = (read-object)
	    do (make-indirect-object number 0 object index)
	    collect (cons number object)))))

;;;# XREF

(defconstant +xref-search-size+ 1024
  "Number of bytes at the end of the stream in which to look for 'startxref'")

(defun find-startxref ()
  (let ((length (stream-length *pdf-stream*))
	(buffer (make-array +xref-search-size+ :element-type 'octet)))
    (file-position *pdf-stream* (- length +xref-search-size+))
    (read-sequence buffer *pdf-stream*)
    (let ((position (search #"startxref" buffer :from-end t)))
      (unless position (error "Can't find file trailer"))
      (parse-integer (octets-latin1 (subseq buffer (+ position 10) (min +xref-search-size+ (+ position 50))))
		     :junk-allowed t))))

(defun read-trailer ()
  (skip-white-space-and-comments)
  (eat-chars #"trailer")
  (skip-white-space-and-comments)
  (eat-char #!<)
  (read-dictionary))

(defun read-cross-reference-entry (number)
  (let ((position (get-integer (read-object))))
    (skip-white-space-and-comments)
    (let ((generation (get-integer (read-object))))
      (skip-white-space-and-comments)
      (let ((type (read-byte *pdf-stream*)))
	(skip-white-space-and-comments)
	(when (= #!n type)
	  (make-indirect-object number generation :uncompressed position))))))

(defun read-cross-reference-subsection ()
  (let ((first (get-integer (read-object)))
	(n (get-integer (read-object))))
    (loop repeat n
	  for number from first
	  do (read-cross-reference-entry number))))

(defun read-cross-reference-subsections (position)
  (file-position *pdf-stream* position)
  (eat-chars #"xref")
  (loop for _ = (skip-white-space-and-comments)
	for char = (peek-byte *pdf-stream*)
	until (= #!t char)
	do (read-cross-reference-subsection)))

(defun read-xref-table (position)
  (let ((first-trailer nil))
    (loop (read-cross-reference-subsections position)
	  (let ((trailer (read-trailer)))
	    (unless first-trailer (setf first-trailer trailer))
	    (let ((prev (get-dict #"Prev" trailer)))
	      (if prev
		  (setf position (get-integer prev))
		  (return first-trailer)))))))

(defun read-int-bytes (stream count)
  (loop for j downfrom (1- count) to 0
	summing (ash (read-byte stream) (* j 8))))

(defun read-xref-stream (position)
  (let* ((xref-stream (read-indirect-object-content position))
	 (size (get-integer (get-dict #"Size" xref-stream)))
	 (w (get-dict #"W" xref-stream)))
    (assert (and (typep w 'pdf-array)
		 (= 3 (get-length w))))
    (let* ((field1 (get-integer (get-array w 0)))
	   (field2 (get-integer (get-array w 1)))
	   (field3 (get-integer (get-array w 2))))
      (let ((s (get-stream xref-stream)))
	(loop for object-number below size
	      do (let ((type 0)
		       (field2-value 0)
		       (field3-value 0))
		   (setf type
		       (if (= 0 field1)
			   1
			   (read-int-bytes s field1)))
		   (setf field2-value (read-int-bytes s field2))
		   (setf field3-value (read-int-bytes s field3))
		   (ecase type
		     (0 ; free - field2-value = next free object number; field3-value = next generation number
		      #+(or)(make-indirect-object number field2-value :free field3-value))
		     (1 ; uncompressed - field2-value = position; field3-value = generation number
		      (make-indirect-object object-number field3-value :uncompressed field2-value))
		     (2 ; compressed - field2-value = object-stream object number; field3-value = index within stream
		      (make-indirect-object object-number field2-value :compressed field3-value)))))))
    (make-instance 'pdf-dictionary :value (pdf-object-value xref-stream))))

(defun read-xref (position)
  (file-position *pdf-stream* position)
  (let ((char (peek-byte *pdf-stream* t)))
    (if (= #!x char)
	(read-xref-table position)
	(read-xref-stream position))))

;;;# DOCUMENT

;; go through xrefs storing position corresponding to (obj gen) pair
;; when an objstrm is referenced, store that
;; when looking up where to find object content, check objstrm list
;; when retrieving content, cache it
;; when dereferencing indirect object, check cache first
;; do we store 'cache' in indirect object itself? can we use :content as :type?
;; if it's :uncompressed or :compressed then must not be retrieved
;; do we store document in object?

(defclass document ()
  ((%trailer
    :initarg :root
    :accessor document-trailer)
   (%indirect-objects
    :initform (make-hash-table :test 'equal)
    :accessor indirect-objects)
   (%objects
    :initarg :objects
    :accessor document-objects)
   (%pages
    :initarg :pages
    :accessor document-pages)))

(defparameter *document* nil)

(defun read-pdf-file (pathname)
  (let ((*document* (make-instance 'document)))
    (with-input-from-octet-file (*pdf-stream* pathname)
      (read-pdf))
    *document*))

(defun collect-pages (page vector)
  (if (nameql #"Pages" (get-dict #"Type" (ensure-object page)))
      (dolist (kid (pdf-object-value (get-dict #"Kids" (ensure-object page))))
	(collect-pages kid vector))
      (vector-push-extend page vector)))

(defun read-pdf ()
  (let ((trailer (read-xref (find-startxref))))
    (setf (document-trailer *document*) trailer)
    (load-all-indirect-objects)
    (let ((root-node (ensure-object (get-dict #"Pages" (ensure-object (get-dict #"Root" trailer)))))
	  (vector (serapeum:vect)))
      (collect-pages root-node vector)
      (setf (document-pages *document*) vector))))
