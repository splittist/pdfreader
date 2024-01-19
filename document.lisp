;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

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
  (let ((object-stream (ensure-object (read-indirect-object stream-number 0))))
    (unless (eq 'pdf-object-stream (type-of object-stream))
      (read-object-stream* object-stream))
    (let* ((*pdf-stream* (get-stream object-stream))
	   (dict (pdf-object-stream-object-dict object-stream))
	   (position (cdr (nth stream-index dict))))
      (file-position *pdf-stream* position)
      (let* ((o (read-object)) ;; DEBUG
	     #+(or)(datum (typecase o
		      (pdf-dictionary (get-dict #"Type" o))
		      (otherwise 0))))
;	(format t "~%~4d ~4d ~A" stream-number stream-index datum)
	o))))

#+(or)(defun read-object-stream (object-stream)
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
    (let* ((pairs (loop repeat n collecting (cons (get-integer (read-object))
						  (get-integer (read-object)))))
	   (dict (loop for (number . offset) in pairs
		       for position = (+ offset first)
		       collecting (cons number position))))
      (change-class object-stream 'pdf-object-stream :object-dict dict))))
	  
;;;# XREF

(defconstant +xref-search-size+ 1024
  "Number of bytes at the end of the stream in which to look for 'startxref'")

(defun find-startxref ()
  (let ((length (stream-length *pdf-stream*))
	(buffer (make-array +xref-search-size+ :element-type 'octet)))
    (file-position *pdf-stream* (max 0 (- length +xref-search-size+)))
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
		      (pushnew field2-value (document-object-streams *document*)) ;; FIXME DEBUG
		      #+(or)(format t "~%~4D ~4D ~4D" object-number field2-value field3-value)
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
   (%object-streams
    :initform '()
    :accessor document-object-streams)
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

(defun load-object-streams ()
  (let ((result '()))
    (dolist (object-number (document-object-streams *document*))
      (push (cons object-number (ensure-object (read-indirect-object object-number 0)))
	    result))
    (setf (document-object-streams *document*) result)))

(defun read-pdf ()
  (let ((trailer (read-xref (find-startxref))))
    (setf (document-trailer *document*) trailer)
;    (load-object-streams)
    (load-all-indirect-objects)
    (let ((root-node (get-dict #"Pages" (get-dict #"Root" trailer)))
	  (vector (serapeum:vect)))
      (collect-pages root-node vector)
      (setf (document-pages *document*) vector))))
