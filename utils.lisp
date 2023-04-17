;;;; cl-pdf-reader

(in-package #:com.splittist.pdfreader)

;;;# OCTETS

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(array octet (,length)))

(defun octets-latin1 (octets)
  (map 'string #'code-char octets))

(defun latin1-octets (string)
  (map '(vector octet) #'char-code string))

(defun digit-octet-p (octet &optional (radix 10)) ;; FIXME?
  (digit-char-p (code-char octet) radix))

(defun octets= (vector1 vector2)
  (not (mismatch vector1 vector2)))

;;;# OCTET-VECTOR-STREAM

(defclass octet-vector-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((%vector
    :initarg :vector
    :accessor vector-stream-vector)
   (%index
    :initarg :index
    :accessor vector-stream-index
    :initform 0)
   (%end
    :initarg :end
    :accessor vector-stream-end)))

(defgeneric peek-byte (stream &optional peek-type eof-error-p eof-value)
  (:method ((stream octet-vector-stream) &optional peek-type (eof-error-p t) eof-value)
    (let ((index (vector-stream-index stream)))
      (loop for octet = (read-byte stream eof-error-p :eof)
	    for new-index from index
	    until (cond ((eq :eof octet)
			 (return eof-value))
			((null peek-type))
			((eq t peek-type)
			 (not (white-space-p octet)))
			((= octet peek-type)))
	    finally (setf (vector-stream-index stream) new-index)
		    (return octet)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream octet-vector-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (with-accessors ((vector vector-stream-vector)
		   (index vector-stream-index)
		   (end vector-stream-end))
      stream
    (if (< index end)
	(prog1 (aref vector index)
	  (incf index))
	:eof)))

(defmethod trivial-gray-streams:stream-listen ((stream octet-vector-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (with-accessors ((index vector-stream-index)
		   (end Vector-stream-end))
      stream
    (< index end)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream octet-vector-stream) sequence start end &key)
  (loop with vector-end = (vector-stream-end stream)
	with vector = (vector-stream-vector stream)
	for index from start below end
	for vector-index = (vector-stream-index stream)
	while (< vector-index vector-end)
	do (let ((elt (aref vector vector-index)))
	     (setf (elt sequence index) elt))
	   (incf (vector-stream-index stream))
	finally (return index)))

(defmethod trivial-gray-streams:stream-file-position ((stream octet-vector-stream))
  (vector-stream-index stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream octet-vector-stream))
  (setf (vector-stream-index stream)
	(case position-spec
	  (:start 0)
	  (:end (vector-stream-end stream))
	  (otherwise
	   (unless (integerp position-spec)
	     (error 'stream-error :stream stream)) ; FIXME better condition
	   (unless (<= 0 position-spec (vector-stream-end stream))
	     (error 'stream-error :stream stream)) ; FIXME better condition
	   position-spec)))
  position-spec)

(defun make-octet-vector-stream (vector &key (start 0) (end (length vector)))
  (check-type vector (vector octet))
  (make-instance 'octet-vector-stream
		 :vector vector
		 :index start
		 :end end))

(defmacro with-input-from-octet-vector ((var vector &key start end) &body body)
  (alexandria:once-only (vector)
    `(let (,var)
       (unwind-protect
	    (progn
	      (setf ,var (make-octet-vector-stream ,vector
						   :start (or ,start 0)
						   :end (or ,end (length ,vector))))
	      ,@body)
	 (when ,var (close ,var))))))

;;;# OCTET-FILE-STREAM

(defclass octet-file-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((%file-stream
    :initarg :file-stream
    :accessor octet-file-stream-file-stream)
   (%peeked-byte
    :accessor peeked-byte
    :initform nil)))

(defmethod initialize-instance :after ((stream octet-file-stream) &key)
  (setf (peeked-byte stream)
	(read-byte (octet-file-stream-file-stream stream) nil :eof)))

(defmethod trivial-gray-streams:stream-read-byte ((stream octet-file-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (shiftf (peeked-byte stream)
	  (read-byte (octet-file-stream-file-stream stream) nil :eof)))

(defmethod trivial-gray-streams:stream-listen ((stream octet-file-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (not (eq :eof (peeked-byte stream))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream octet-file-stream) sequence start end &key)
  (loop for index from start below end
	for char = (read-byte stream nil :eof)
	until (eq :eof char)
	do (setf (elt sequence index) char)
	finally (return index)))

(defmethod trivial-gray-streams:stream-file-position ((stream octet-file-stream))
  (file-position (octet-file-stream-file-stream stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream octet-file-stream))
  (unless (integerp position-spec) (error 'stream-error :stream stream))
  (file-position (octet-file-stream-file-stream stream) position-spec)
  (setf (peeked-byte stream) (read-byte (octet-file-stream-file-stream stream)))
  position-spec)

(defmethod peek-byte ((stream octet-file-stream) &optional peek-type eof-error-p eof-value)
  (loop for octet = (peeked-byte stream)
	until (cond ((eq :eof octet)
		     (return eof-value))
		    ((null peek-type))
		    ((eq t peek-type)
		     (not (white-space-p octet)))
		    ((= octet peek-type)))
	do (read-byte stream eof-error-p :eof)
	finally (return octet)))

(defun make-octet-file-stream (filespec)
  (make-instance 'octet-file-stream
		 :file-stream (open filespec :element-type 'octet)))

(defmacro with-input-from-octet-file ((var filespec) &body body)
  (alexandria:with-gensyms (file-stream)
    `(let (,var ,file-stream)
       (unwind-protect
	(progn
	  (setf ,file-stream (open ,filespec :element-type 'octet)
		,var (make-instance 'octet-file-stream :file-stream ,file-stream))
	  ,@body)
	 (when ,file-stream (close ,file-stream))
	 (when ,var (close ,var))))))

;;;# STREAM-LENGTH

(defun stream-length (stream)
  (etypecase stream
    ((or file-stream synonym-stream broadcast-stream)
     (file-length stream))
    (octet-vector-stream
     (vector-stream-end stream))
    (octet-file-stream
     (+ (file-length (octet-file-stream-file-stream stream))
	(if (eq :eof (peeked-byte stream)) 0 1)))))

;;;# TEXT MATRICES

(defvar +identity-text-matrix+ #(1 0 0 0 1 0 0 0 1))

(defun make-matrix (&optional (initial-contents +identity-text-matrix+))
  (make-array 9 :initial-contents initial-contents))

(defun make-translation-matrix (tx ty)
  (make-array 9 :initial-contents (list 1 0 0 0 1 0 tx ty 1)))

(defun m* (m n)
  "Multiply two 3 x 3 matrices represented as 9 length vectors"
  (assert (= 9 (length m) (length n)))
  (vector (+ (* (svref m 0) (svref n 0))
	     (* (svref m 1) (svref n 3))
	     (* (svref m 2) (svref n 6)))
	  (+ (* (svref m 0) (svref n 1))
	     (* (svref m 1) (svref n 4))
	     (* (svref m 2) (svref n 7)))
	  (+ (* (svref m 0) (svref n 2))
	     (* (svref m 1) (svref n 5))
	     (* (svref m 2) (svref n 8)))
	  (+ (* (svref m 3) (svref n 0))
	     (* (svref m 4) (svref n 3))
	     (* (svref m 5) (svref n 6)))
	  (+ (* (svref m 3) (svref n 1))
	     (* (svref m 4) (svref n 4))
	     (* (svref m 5) (svref n 7)))
	  (+ (* (svref m 3) (svref n 2))
	     (* (svref m 4) (svref n 5))
	     (* (svref m 5) (svref n 8)))
	  (+ (* (svref m 6) (svref n 0))
	     (* (svref m 7) (svref n 3))
	     (* (svref m 8) (svref n 6)))
	  (+ (* (svref m 6) (svref n 1))
	     (* (svref m 7) (svref n 4))
	     (* (svref m 8) (svref n 7)))
	  (+ (* (svref m 6) (svref n 2))
	     (* (svref m 7) (svref n 5))
	     (* (svref m 8) (svref n 8)))))

;;;# PAPER SIZES

(defparameter *paper-sizes*
  '(((:a0) . (2384 . 3370))
    ((:a1) . (1684 . 2384))
    ((:a2) . (1190 . 1684))
    ((:a3) . ( 842 . 1190))
    ((:a4) . ( 595 .  842))
    ((:a5) . ( 420 .  595))
    ((:a6) . ( 298 .  420))
    ((:a7) . ( 210 .  298))
    ((:a8) . ( 148 .  210))

    ((:b0) . (2835 . 4008))
    ((:b1) . (2004 . 2835))
    ((:b2) . (1417 . 2004))
    ((:b3) . (1001 . 1417))
    ((:b4) . ( 709 . 1001))
    ((:b5) . ( 499 .  709))
    ((:b6) . ( 354 .  499))
    ((:b7) . ( 249 .  354))
    ((:b8) . ( 176 .  249))
    ((:b9) . ( 125 .  176))
    ((:b10) . ( 88 .  125))

    ((:c2) . (1837 .  578))
    ((:c3) . ( 578 .  919))
    ((:c4) . ( 919 .  649))
    ((:c5) . ( 649 .  459))
    ((:c6) . ( 459 .  323))

    ((:d0) . (3090 . 2186))

    ((:sra0) . (3628 . 2551))
    ((:sra1) . (2551 . 1814))
    ((:sra2) . (1814 . 1276))
    ((:sra3) . (1276 .  907))
    ((:sra4) . ( 907 .  638))
    ((:ra0)  . (3458 . 2438))
    ((:ra1)  . (2438 . 1729))
    ((:ra2)  . (1729 . 1219))

    ((:letter :ansi-a) . (612 . 792))
    ((:legal) . (612 . 1008))
    ((:ledger :ansi-b) . (792 . 1224))
    ((:tabloid) . (1224. 792))
    ((:executive) . (522 . 756))
    ((:ansi-c) . (1584 . 1224))
    ((:ansi-d) . (2448 . 1584))
    ((:ansi-e) . (3168 . 2448))

    ((:foolscap) . (954 . 1188))
    ((:small-post) . (1044 . 1332))
    )
  "Paper sizes: ((Names..) (width . height)) in points")

(defun paper-size-width-height (name &optional (orientation :portrait))
  (alexandria:when-let ((result (serapeum:assocdr name *paper-sizes* :test 'find)))
    (ecase orientation
      ((:portrait :upside-down) (values (car result) (cdr result)))
      ((:landscape :seascape) (values (cdr result) (car result))))))

(defun points-pixels (points &optional (ppi 300))
  (* (/ points 72) ppi))

;;;# COLOURS

(defun cmyk-rgb (cmyk)
  (let ((c (colored:c cmyk))
	(m (colored:m cmyk))
	(y (colored:y cmyk))
	(k (colored:k cmyk)))
    (setf c (+ k (* c (- 1 k)))
	  m (+ k (* m (- 1 k)))
	  y (+ k (* y (- 1 k))))
    (colored:rgb (- 1 c) (- 1 m) (- 1 y))))
