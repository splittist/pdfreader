;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;; FILTERS

(defparameter *filters*
  '(("ASCIIHexDecode" ascii-hex-decode)
    ("ASCII85Decode" ascii-85-decode)
    ("LZWDecode" lzw-decode "Predictor" "Colors" "BitsPerComponent" "Columns" "EarlyChange")
    ("FlateDecode" flate-decode "Predictor" "Colors" "BitsPerComponent" "Columns")
    ("RunLengthDecode" run-length-decode)
    ("CCITTFaxDecode" ccit-fax-decode "K" "EndOfLine" "EncodedByteAlign" "Columns" "Rows" "EndOfBlock" "BlackIs1" "DamagedRowsBeforeError")
    ("JBIG2Decode" jbig2-decode "JBIG2Globals")
    ("DCTDecode" dct-decode "ColorTransform")
    ("JPXDecode" jpx-decode)
    ("Crypt" crypt "Type" "Name")))

(defclass filter ()
  ((%base-stream
    :initarg :stream
    :accessor filter-base-stream)))

(defun make-filter (stream filter params)
  (setf filter (ensure-object filter)
	params (ensure-object params))
  (unless (typep filter 'pdf-name) (error "Bad thing to be a Filter name: ~S" filter))
  #+(or)(unless (and (not (null params))
	       (typep params 'pdf-dictionary))
    (error "Bad thing to be DecodeParms: ~S" params))
  ;; FIXME ignoring predictor for now
  (cond
    ((nameql filter #"FlateDecode")
     (let ((flate (make-instance 'flate-decode :stream stream)))
       (if (null params)
	   flate
	   ;; FIXME assumes PNG
	   (let* ((flate-vector (vector-stream-vector flate))
		  (columns (get-integer (get-dict #"Columns" params)))
		  (predicted-vector (png-predictor flate-vector columns)))
	     (make-octet-vector-stream predicted-vector)))))
    (t
     (error "Filter not implemented: ~A" filter))))

(defgeneric decode (object filter))

(defclass ascii-hex-decode (filter)
  ())

(defmethod decode ((string string) (filter ascii-hex-decode))
  (loop with prev = 0
	with result = '()
	with count = 1
	for char across string
	for hex-value = (digit-char-p char 16)
	do (cond ((white-char-p char)
		  nil)
		 ((and hex-value (oddp count))
		  (setf prev hex-value)
		  (incf count))
		 ((and hex-value (evenp count))
		  (push (+ (* 16 prev) hex-value) result)
		  (incf count))
		 ((char= #\> char)
		  (when (evenp count)
		    (push (* 16 prev) result))
		  (return (apply 'vector (nreverse result))))
		 (t (error "Unknown char in ASCIIHEXDecode: ~C" char)))))
		  
(defclass ascii-85-decode (filter)
  ())

(defmethod decode ((string string) (filter ascii-85-decode))
  (let ((accum '())
	(count 0)
	(result '())
	(tilde-seen nil))
    (flet ((push-to-result ()
	     (let ((num (loop for i below count
			      summing (* (elt accum i) (expt 85 i)))))
	       (loop for i downfrom (- count 2) to 0
		     do (push (ldb (byte 8 (* i 8)) num) result)))))
      (loop for char across string
	    for code = (char-code char)
	    do (cond ((white-char-p char)
		      nil)
		     ((char= #\~ char)
		      (setf tilde-seen t))
		     ((and tilde-seen (char= #\> char))
		      (loop-finish))
		     ((<= #.(char-code #\!) code #.(char-code #\u))
		      (push (- code 33) accum)
		      (incf count)
		      (when (= count 5)
			(push-to-result)
			(setf count 0
			      accum '())))
		     ((char= #\z char)
		      (unless (zerop count)
			(error "Saw #\z in middle of group"))
		      (loop repeat 4
			    do (push 0 result))))
	    finally (unless (zerop count)
		      (loop until (= count 5)
			    do (push 0 accum)
			       (incf count))
		      (push-to-result))
		    (return (apply 'vector (nreverse result)))))))

;;; lzw-decode - parameters, predictor
;;;  parameters: /Predictor (integer, default 1)
;;;              /Colors (integer, default 1)
;;;              /BitsPerComponent (integer, default 8)
;;;              /Columns (integer, default 1)
;;;              /EarlyChange (integer, default 1)

;;; 9-12 bits
;;;  0-255 character
;;;  256 clear table
;;;  257 EOD
;;;  258+ table entry

;;; Predictors
;;;  1  No prediction (Default)
;;;  2  TIFF Predictor 2
;;; 10  PNG prediction, PNG None on all rows
;;; 11  PNG prediction, PNG Sub on all rows
;;; 12  PNG prediction, PNG Up on all rows
;;; 13  PNG prediction, PNG Average on all rows
;;; 14  PNG prediction, PNG Paeth on all rows
;;; 15  PNG prediction, PNG optimum ??

;;; It seems that 10-15 are actually the same, with the encoding/decoding strategy
;;; decided on a per-row basis, so all of these need to be combined.

(defun none-predictor (left above upper-left)
  (declare (ignore left above upper-left))
  0)

(defun sub-predictor (left above upper-left)
  (declare (ignore above upper-left))
  left)

(defun up-predictor (left above upper-left)
  (declare (ignore left upper-left))
  above)

(defun average-predictor (left above upper-left)
  (declare (ignore upper-left))
  (floor (+ left above) 2))

(defun paeth-predictor (left above upper-left)
  (let* ((initial-estimate (- (+ left above) upper-left))
	 (left-distance (abs (- initial-estimate left)))
	 (above-distance (abs (- initial-estimate above)))
	 (upper-left-distance (abs (- initial-estimate upper-left))))
    (cond ((and (<= left-distance above-distance)
		(<= left-distance upper-left-distance))
	   left)
	  ((<= above-distance upper-left-distance)
	   above)
	  (t
	   upper-left))))

(defun png-predictor (vector columns)
  (loop with result = '()
        with prior = (make-array columns :initial-element 0)
	with prev = 0
        for index by (1+ columns) below (- (length vector) columns)
        do (let ((type-byte (aref vector index)))
             (assert (<= 0 type-byte 4))
	     (setf prev 0)
             (loop for offset below columns
                   for i from (1+ index)
                   for byte = (aref vector i)
                   for new = (logand
			      (+ byte
				 (funcall
				  (case type-byte
				    (0 'none-predictor)
				    (1 'sub-predictor)
				    (2 'up-predictor)
				    (3 'average-predictor)
				    (4 'paeth-predictor))
				  prev
				  (aref prior offset)
				  (if (zerop offset)
				      0
				      (aref prior (1- offset)))))
				 #xFF)
                   do (push new result)
                      (setf (aref prior offset) new)))
        finally (return (make-array (length result) :element-type 'octet :initial-contents (nreverse result)))))

;;; flate-decode - parameters, predictor
;;;  parameters as for LZW, but NO /EarlyChange

(defclass flate-decode (filter octet-vector-stream)
  ())

(defmethod initialize-instance :after ((obj flate-decode) &key)
  (let* ((base-stream (filter-base-stream obj))
	 (length (stream-length (filter-base-stream obj)))
	 (base-vector (make-array length :element-type 'octet))
	 (read-length (read-sequence base-vector base-stream)))
    (unless (eql length read-length) (error "Flate decode read error"))
    (setf (vector-stream-vector obj) (semz.decompress:decompress :deflate base-vector :start 2)
	  (vector-stream-index obj) 0
	  (vector-stream-end obj) (length (vector-stream-vector obj)))))

(defmethod decode ((string string) (filter flate-decode))
  (decode (latin1-octets string) filter))

(defmethod decode ((vector vector) (filter flate-decode))
  (semz.decompress:decompress :deflate vector :start 2))

;;; run-length-decode

(defclass run-length-decode (filter)
  ())

;; FIXME - test, almost certainly wrong
(defmethod decode ((string string) (filter run-length-decode))
  (let ((result '()))
    (loop with index = 0
	  for char = (char string index)
	  for code = (char-code char)
	  do (cond ((= code 128)
		    (loop-finish))
		   ((<= 0 code 127)
		    (loop repeat (1+ code)
			  for i from (1+ index)
			  for char = (char string i)
			  do (push char result)
			  finally (setf index (1+ i))))
		   (t
		    (loop with repeated-char = (char string (1+ index))
			  repeat (- 257 code)
			  do (push repeated-char result)
			  finally (incf index 2))))
	  finally (return (apply 'vector (nreverse result))))))

;;; ccitt-fax-decode - parameters
;;; /K (integer, default 0)
;;;   < 0 Group 4, pure 2D
;;;   = 0 Group 3, pure 1D
;;;   > 0 Group 3, mixed 2D, etc.
;;; /EndOfLIne (boolean, default false)
;;; /EncodedByteAlign (boolean, default false)
;;; /Columns (integer, default 1728)
;;; /Rows (integer, default 0)
;;; /EndOfBlock (boolean, default true)
;;; /BlackIs1 (boolean, default false)
;;; /DamagedRowsBeforeError (integer, default 0)

;;; jbig2-decode - parameters
;;; /JBIG2Globals (stream)

;;; dct-decode - parameters
;;; /ColorTransform (ingeger, default 1 if three components and 0 otherwise)

;;; jpx-decode (only image XObjects)

;;; crypt - parameters
;;; /Type (name - /CryptFilterDecodeParms)
;;; /Name (name - corresponding to /CF entry - table 26)

