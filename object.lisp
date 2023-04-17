;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;;# PDF OBJECTS

(defclass pdf-object ()
  ())

(defclass pdf-value-object (pdf-object)
  ((%value
    :initarg :value
    :accessor pdf-object-value)))

(defmethod print-object ((object pdf-value-object) stream)
  (if (not *print-escape*)
      (princ (pdf-object-value object) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(princ (pdf-object-value object) stream))))

(defgeneric get-length (object))

;;;## PDF-KEYWORD

(defclass pdf-keyword (pdf-value-object)
  ())

(defmethod print-object ((object pdf-keyword) stream)
  (if (not *print-escape*)
      (princ (octets-latin1 (pdf-object-value object)) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(princ (octets-latin1 (pdf-object-value object)) stream))))

;;;## PDF-BOOLEAN

(defclass pdf-boolean (pdf-keyword)
  ())

(defparameter +true+ (make-instance 'pdf-boolean :value #"true"))

(defparameter +false+ (make-instance 'pdf-boolean :value #"false"))

;;;## PDF-NUMBER

(defclass pdf-number (pdf-value-object)
  ())

(defun get-number (object)
  (setf object (ensure-object object))
  (etypecase object
    (pdf-number
     (pdf-object-value object))
    (number
     object)))

(defun get-integer (object)
  (setf object (ensure-object object))
  (if (and (typep object 'pdf-number)
	   (integerp (pdf-object-value object)))
      (pdf-object-value object)
      (if (integerp object)
	  object
	  (error 'type-error :expected-type 'integer  :datum object))))

;;;## PDF-STRING

(defclass pdf-string (pdf-value-object)
  ())

(defmethod print-object ((object pdf-string) stream)
  (if (not *print-escape*)
      (format stream "\"~A\"" (octets-latin1 (pdf-object-value object)))
      (print-unreadable-object (object stream :type t :identity t)
	(let ((string (octets-latin1 (pdf-object-value object))))
	  (princ (serapeum:ellipsize string 10) stream)))))

(defmethod get-length ((object pdf-string))
  (length (pdf-object-value object)))

(defun get-string (object)
  (setf object (ensure-object object))
  (etypecase object
    (pdf-string
     (pdf-object-value object))
    (octet-vector
     object)))

;;;## PDF-NAME

(defclass pdf-name (pdf-value-object)
  ())

(defmethod print-object ((object pdf-name) stream)
  (if (not *print-escape*)
      (format stream "/~A" (octets-latin1 (pdf-object-value object)))
      (print-unreadable-object (object stream :type t :identity t)
	(princ (octets-latin1 (pdf-object-value object)) stream))))

(defparameter *names* (make-hash-table :test 'equalp))

(defun intern-name (octets)
  (alexandria:if-let ((existing (gethash octets *names*)))
    existing
    (setf (gethash octets *names*) (make-instance 'pdf-name :value octets))))

(defgeneric nameql (thing1 thing2)
  (:method ((thing1 pdf-name) (thing2 pdf-name))
    (eql thing1 thing2))
  (:method (thing1 (thing2 pdf-name))
    (and (typep thing1 'octet-vector)
	 (eql (intern-name thing1) thing2)))
  (:method ((thing1 pdf-name) thing2)
    (and (typep thing2 'octet-vector)
	 (eql thing1 (intern-name thing2)))))

;;;## PDF-NULL

(defclass pdf-null (pdf-keyword)
  ())

(defparameter +null+ (make-instance 'pdf-null :value #"null"))

;;;## PDF-ARRAY

(defclass pdf-array (pdf-value-object)
  ())

(defmethod print-object ((object pdf-array) stream)
  (if (not *print-escape*)
      (format stream "[~S]" (pdf-object-value object))
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~D items" (length (pdf-object-value object))))))

(defgeneric get-array (array index)
  (:method ((array pdf-array) index)
    (nth index (pdf-object-value array))))

(defgeneric get-vector (object)
  (:method ((object pdf-array))
    (apply 'vector (pdf-object-value object))))

(defmethod get-length ((object pdf-array))
  (length (pdf-object-value object)))

;;;## PDF-DICTIONARY

(defclass pdf-dictionary (pdf-value-object)
  ())

(defmethod print-object ((object pdf-dictionary) stream)
  (if (not *print-escape*)
      (format stream "<<~S>>" (pdf-object-value object))
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~D items" (length (pdf-object-value object))))))

;; FIXME for non-name dictionaries
(defgeneric get-dict (key dictionary)
  (:method ((key vector) (dictionary pdf-dictionary))
    (get-dict (intern-name key) dictionary))
  (:method ((key pdf-object) (dictionary pdf-dictionary))
    (let ((value (ensure-object (serapeum:assocdr key (pdf-object-value dictionary)))))
      (if (eq +null+ value)
	  nil
	  value))))

;; doesn't really work for non-pdf-value keys
(defgeneric (setf get-dict) (value key dictionary) ; FIXME multiple evaluation?
  (:method ((value pdf-object) (key pdf-object) (dictionary pdf-dictionary))
    (let ((pair (nth-value 1 (get-dict key dictionary))))
      (if pair
	  (setf (cdr pair) value)
	  (push (cons key value) (pdf-object-value dictionary))))))

;;;## PDF-STREAM

(defclass pdf-stream (pdf-dictionary)
  ((%content
    :initarg :content
    :accessor pdf-stream-content)
   (%stream
    :accessor pdf-stream-stream
    :initform nil)))

(defgeneric get-stream (object)
  (:method ((pdf-stream pdf-stream))
    (alexandria:if-let ((s (pdf-stream-stream pdf-stream)))
      s
      (let ((stream (make-octet-vector-stream (pdf-stream-content pdf-stream)))
	    ;; PDF 2.0 - short names override long ones, apparently
	    (filters (or (get-dict #"F" pdf-stream)
			 (get-dict #"Filter" pdf-stream)))
	    (params (or (get-dict #"DP" pdf-stream)
			(get-dict #"DecodeParms" pdf-stream))))
	(cond
	  ((null filters))
	  ((typep filters 'pdf-name)
	   (setf stream (make-filter stream filters params)))
	  ((typep filters 'pdf-array)
	   (let ((filter-list (pdf-object-value filters))
		 (ps (pdf-object-value params)))
	     (dotimes (i (length filter-list))
	       (let ((filter (elt filter-list i))
		     (p (if (listp ps)
			    (elt ps i)
			    ps)))
		 (setf stream (make-filter stream filter p)))))))
	(setf (pdf-stream-stream pdf-stream) stream)))))

;;;# OBJECT-STREAM

(defclass object-stream ()
  ((%number
    :initarg :number
    :accessor object-stream-number)
   (%objects
    :initarg :objects
    :accessor object-stream-dict
    :initform nil)))

(defun make-object-stream (number objects)
  (make-instance 'object-stream :number number :objects objects))

(defun get-object-stream (object-stream index)
  (cdr (nth index (object-stream-dict object-stream))))

(defmethod print-object ((object object-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D (~D)"
	    (object-stream-number object)
	    (length (object-stream-dict object)))))

;;;# INDIRECT-OBJECT

(defclass indirect-object (pdf-object)
  ((%number
    :initarg :number
    :accessor object-number)
   (%generation
    :initarg :generation
    :accessor generation-number)
   ;; one of :uncompressed, :compressed, or the actual content.
   (%content
    :initarg :content
    :accessor content
    :initform nil)))

(defmethod print-object ((object indirect-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D ~D ~A"
	    (object-number object)
	    (generation-number object)
	    (if (keywordp (content object))
		(content object)
		""))))

(defun make-indirect-object (object-number generation-number type position)
  (serapeum:lret ((object (or (car (gethash (cons object-number generation-number) (indirect-objects *document*)))
			      (make-instance 'indirect-object
					     :number object-number
					     :generation generation-number
					     :content type))))
    (setf (gethash (cons object-number generation-number) (indirect-objects *document*))
	  (cons object position))))

(defun fetch-indirect-object (object-number generation-number)
  (alexandria:if-let ((object.pos (gethash (cons object-number generation-number) (indirect-objects *document*))))
    (values (car object.pos) (cdr object.pos))
    (make-indirect-object object-number generation-number :uncompressed 0))) ; FIXME

(defun read-indirect-object (object-number generation-number)
  (multiple-value-bind (object position) (fetch-indirect-object object-number generation-number)
    (when (keywordp (content object))
      (if (eq :uncompressed (content object))
	  (setf (content object) (read-indirect-object-content position))
	  (setf (content object) (read-indirect-object-content-from-stream
				  (generation-number object)
				  position))))
    object))

(defun load-indirect-object (object)
  (when (keywordp (content object))
    (read-indirect-object (object-number object) (generation-number object)))
  object)

(defun load-all-indirect-objects ()
  (dolist (object.pos (alexandria:hash-table-values (indirect-objects *document*)))
    (load-indirect-object (car object.pos))))

;; unused?
(defun delete-indirect-object (object)
  (remhash (cons (object-number object) (generation-number object)) (indirect-objects *document*)))

(defun ensure-object (thing)
  (typecase thing
    (indirect-object
      ;;(content thing)
     (ensure-object (content (load-indirect-object thing))))
    (pdf-stream
     (let ((type (get-dict #"Type" thing)))
       (if (and type (nameql #"ObjStm" type))
	   (progn (print "ensure-object pdf-stream") #+(or)(break) (read-object-stream thing))
	   thing)))
    (otherwise
     thing)))
