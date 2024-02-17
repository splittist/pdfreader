;;;; pdfreader

(defpackage #:com.splittist.pdfreader.ccitt
  (:use #:cl)
  (:local-nicknames (#:pdf #:com.splittist.pdfreader))
  #+(or)(:export )
  )

(in-package #:com.splittist.pdfreader.ccitt)
 #|
(defclass bit-writer ()
  ((%data
    :initarg :data
    :accessor bit-writer-data)))

(defun make-bit-writer ()
  (make-instance 'bit-writer :data (make-array 0 :element-type 'bit :adjustable t)))

(defgeneric bit-write (writer thing)
  (:method ((writer bit-writer) (number integer))
	   (loop for i from (1- (integer-length number)) downto 0
		 for bit = (ldb (byte 1 i) number)
		 do (vector-push-extend bit (bit-writer-data writer)))
	   writer))

(defclass bit-reader ()
  ((%data
    :initarg :data
    :accessor bit-reader-data)
   (%index
    :initform 0
    :accessor bit-reader-index)))

(defun make-bit-reader (data)
  (check-type data bit-vector)
  (make-instance 'bit-reader :data data))

(defgeneric end-of-data-p (reader)
  (:method ((reader bit-reader))
    (>= (bit-reader-index reader) (length (bit-reader-data reader)))))

(defgeneric bit-peek (reader length)
  (:method ((reader bit-reader) (length integer))
    (unless (plusp length)
      (error "Invalid length: ~D" length))
    (with-accessors ((index bit-reader-index) (data bit-reader-data))
	reader
      (when (>= (+ index length) (length data))
	(error "Insufficient data"))
      (subseq data index (+ index length)))))

(defgeneric bit-read (reader length)
  (:method ((reader bit-reader) (length integer))
    (prog1
	(bit-peek reader length)
      (incf (bit-reader-index reader) length))))

(defgeneric bit-looking-at (reader bits)
  (:method ((reader bit-reader) (bits bit-vector))
    (equal bits (bit-peek reader (length bits)))))

(defparameter +eol+ #*000000000001)

(defparameter +eob+ #*000000000001000000000001000000000001000000000001000000000001000000000001) ;; +eol+ x 6

(defparameter +white-terminals+
  #(#*00110101
    #*000111
    #*0111
    #*1000
    #*1011
    #*1100
    #*1110
    #*1111
    #*10011
    #*10100
    #*00111
    #*01000
    #*001000
    #*000011
    #*110100
    #*110101
    #*101010
    #*101011
    #*0100111
    #*0001100
    #*0001000
    #*0010111
    #*0000011
    #*0000100
    #*0101000
    #*0101011
    #*0010011
    #*0100100
    #*0011000
    #*00000010
    #*00000011
    #*00011010
    #*00011011
    #*00010010
    #*00010011
    #*00010100
    #*00010101
    #*00010110
    #*00010111
    #*00101000
    #*00101001
    #*00101010
    #*00101011
    #*00101100
    #*00101101
    #*00000100
    #*00000101
    #*00001010
    #*00001011
    #*01010010
    #*01010011
    #*01010100
    #*01010101
    #*00100100
    #*00100101
    #*01011000
    #*01011001
    #*01011010
    #*01011011
    #*01001010
    #*01001011
    #*00110010
    #*00110011
    #*00110100))

(defparameter +black-terminals+
  #(#*0000110111
    #*010
    #*11
    #*10
    #*011
    #*0011
    #*0010
    #*00011
    #*000101
    #*000100
    #*0000100
    #*0000101
    #*0000111
    #*00000100
    #*00000111
    #*000011000
    #*0000010111
    #*0000011000
    #*0000001000
    #*00001100111
    #*00001101000
    #*00001101100
    #*00000110111
    #*00000101000
    #*00000010111
    #*00000011000
    #*000011001010
    #*000011001011
    #*000011001100
    #*000011001101
    #*000001101000
    #*000001101001
    #*000001101010
    #*000001101011
    #*000011010010
    #*000011010011
    #*000011010100
    #*000011010101
    #*000011010110
    #*000011010111
    #*000001101100
    #*000001101101
    #*000011011010
    #*000011011011
    #*000001010100
    #*000001010101
    #*000001010110
    #*000001010111
    #*000001100100
    #*000001100101
    #*000001010010
    #*000001010011
    #*000000100100
    #*000000110111
    #*000000111000
    #*000000100111
    #*000000101000
    #*000001011000
    #*000001011001
    #*000000101011
    #*000000101100
    #*000001011010
    #*000001100110
    #*000001100111))

(defparameter +white-configurations+
  (serapeum:dict
   #*11011 64
   #*10010 128
   #*010111 192
   #*0110111 256
   #*00110110 320
   #*00110111 384
   #*01100100 448
   #*01100101 512
   #*01101000 576
   #*01100111 640
   #*011001100 704
   #*011001101 768
   #*011010010 832
   #*011010011 896
   #*011010100 960
   #*011010101 1024
   #*011010110 1088
   #*011010111 1152
   #*011011000 1216
   #*011011001 1280
   #*011011010 1344
   #*011011011 1408
   #*010011000 1472
   #*010011001 1536
   #*010011010 1600
   #*011000 1664
   #*010011011 1728
   
   #*00000001000 1792
   #*00000001100 1856
   #*00000001001 1920
   #*000000010010 1984
   #*000000010011 2048
   #*000000010100 2112
   #*000000010101 2176
   #*000000010110 2240
   #*000000010111 2340
   #*000000011100 2368
   #*000000011101 2432
   #*000000011110 2496
   #*000000011111 2560))

(defparameter +black-configurations+
  (serapeum:dict
   #*0000001111 64
   #*000011001000 128
   #*000011001001 192
   #*000001011011 256
   #*000000110011 320
   #*000000110100 384
   #*000000110101 448
   #*0000001101100 512
   #*0000001101101 576
   #*0000001001010 640
   #*0000001001011 704
   #*0000001001100 768
   #*0000001001101 832
   #*0000001110010 896
   #*0000001110011 960
   #*0000001110100 1024
   #*0000001110101 1088
   #*0000001110110 1152
   #*0000001110111 1216
   #*0000001010010 1280
   #*0000001010011 1344
   #*0000001010100 1408
   #*0000001010101 1472
   #*0000001011010 1536
   #*0000001011011 1600
   #*0000001100100 1664
   #*0000001100101 1728

   #*00000001000 1792
   #*00000001100 1856
   #*00000001001 1920
   #*000000010010 1984
   #*000000010011 2048
   #*000000010100 2112
   #*000000010101 2176
   #*000000010110 2240
   #*000000010111 2340
   #*000000011100 2368
   #*000000011101 2432
   #*000000011110 2496
   #*000000011111 2560))

(defun color-bits (reader configurations terminals)
  (let ((bits 0)
	(check-configuration t))
    (loop while check-configuration
	  do (setf check-configuration nil)
	     (loop for i from 2 below 14
		   for codeword = (bit-peek reader i)
		   for config-value = (gethash codeword configurations)
		   when config-value
		     do (incf (bit-reader-index reader))
			(incf bits config-value)
			(when (= 2560 config-value)
			  (setf check-configuration t))
			(loop-finish))
	     (loop for i from 2 below 14
		   for codeword = (bit-peek reader i)
		   for term-value = (position codeword terminals)
		   when term-value
		     do (incf (bit-reader-index reader))
			(incf bits term-value)
			(return-from color-bits bits)))
    nil))

(defun black-bits (reader)
  (color-bits reader +black-configurations+ +black-terminals+))

(defun white-bits (reader)
  (color-bits reader +white-configurations+ +white-terminals+))

(defun decode (data &key eol (byte-align t) (columns 1728) (rows 0) (eob t) black-is-1)
  (let ((white 1)
	(black 0)
	(reader (make-bit-reader data))
	(writer (make-bit-writer)))
    (when black-is-1
      (rotatef white black))
    (with-accessors ((index bit-reader-index)) reader
      (loop with current-color
	    until (or (= rows 0)
		      (end-of-data-p reader))
	    do (setf current-color white)
	       (when (and byte-align (not (zerop (mod index 8))))
		 (incf index (- 8 (mod index 8))))
	       (when (and eob (bit-looking-at reader +eob+))
		 (incf index (length +eob+))
		 (loop-finish))
	       (if (not (bit-looking-at reader +eol+))
		   (when eol
		     (error "No end of line pattern at ~D" index))
		   (incf index (length +eol+)))
	       (let ((line-length 0))
		 (loop while (< line-length columns)
		       do (let ((bit-length (if (= white current-color)
						(white-bits reader)
						(black-bits reader))))
			    (when (null bit-length)
			      (error "Unfinished line at ~D" index))
			    (incf line-length bit-length)
			    (when (> line-length columns)
			      (error "Line is too long at ~D" index))
			    (bit-write writer (- (ash current-color bit-length) current-color))
			    (setf current-color (logand current-color 1)))))
	       (decf rows)))
    (bit-writer-data writer)))
|#			      
;;; Completely different implementation

(defparameter +black-codes+
  '((#x2 #x3)
    (#x2 #x3)
    (#x2 #x3)
    (#x3)
    (#x4 #x5)
    (#x4 #x5 #x7)
    (#x4 #x7)
    (#x18)
    (#x17 #x18 #x37 #x8 #xF)
    (#x17 #x18 #x28 #x37 #x67 #x68 #x6C #x8 #xC #xD)
    (#x12 #x13 #x14 #x15 #x16 #x17 #x1C #x1D #x1E #x1F #x24 #x27 #x28 #x2B #x2C #X33
     #x34 #x35 #x37 #x38 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x5B #x64 #x65
     #x66 #x67 #x68 #x69 #x6A #x6B #x6C #x6D #xC8 #xC9 #xCA #xCB #xCC #xCD #xD2 #xD3
     #xD4 #xD5 #xD6 #xD7 #xDA #xDB)
    (#x4A #x4B #x4C #x4D #x52 #x53 #x54 #x55 #x5A #x5B #x64 #x65 #x6C #x6D #x72 #x73
     #x74 #x75 #x76 #x77)))

(defparameter +black-run-lengths+
  '((3 2)
    (1 4)
    (6 5)
    (7)
    (9 8)
    (10 11 12)
    (13 14)
    (15)
    (16 17 0 18 64)
    (24 25 23 22 29 20 21 1792 1856 1920)
    (1984 2048 2112 2176 2240 2304 2368 2432 2496 2560 52 55 56 59 60 320 384 448 53
     54 50 51 44 45 46 47 57 58 61 256 48 49 62 63 30 31 32 33 40 41 128 192 26
     27 28 29 34 35 36 37 38 39 42 43)
    (640 704 768 832 1280 1344 1408 1472 1536 1600 1664 1728 512 576 896 960 1024 1088
     1152 1216)))

(defparameter +white-codes+
  '((#x7 #x8 #xB #xC #xE #xF)
    (#x12 #x13 #x14 #x1B #x7 #x8)
    (#x17 #x18 #x2A #x2B #x3 #x34 #x35 #x7 #x8)
    (#x13 #x17 #x18 #x24 #x27 #x28 #x2B #x3 #x37 #x4 #x8 #xC)
    (#x12 #x13 #x14 #x15 #x16 #x17 #x1A #x1B #x2 #x24 #x25 #x28 #x29 #x2A #x2B #x2C #x2D
     #x3 #x32 #x33 #x34 #x35 #x36 #x37 #x4 #x4A #x4B #x5 #x52 #x53 #x54 #x55 #x58 #x59
     #x5A #x5B #x64 #x65 #x67 #x68 #xA #xB)
    (#x98 #x99 #x9A #x9B #xCC #xCD #xD2 #xD3 #xD4 #xD5 #xD6 #xD7 #xD8 #xD9 #xDA #xDB)
    ()
    (#x8 #xC #xD)
    (#x12 #x13 #x14 #x15 #x16 #x17 #x1C #x1D #x1E #x1F)))

(defparameter +white-run-lengths+
  '((2 3 4 5 6 7)
    (128 8 9 64 10 11)
    (192 1664 16 17 133 14 15 1 12)
    (26 21 28 27 18 24 25 22 256 23 20 19)
    (33 34 35 36 37 38 31 32 29 53 54 39 40 41 42 43 44 30 61 62 63 0 320 384 45
     59 60 46 49 50 51 52 55 56 57 58 448 512 640 576 47 48)
    (1472 1536 1600 1728 704 768 832 896 960 1024 1088 1152 1216 1280 1344 1408)
    ()
    (1792 1856 1920)
    (1984 2048 2112 2176 2240 2304 2368 2432 2496 2560)))

(defclass node ()
  ((%left
    :initarg :left
    :initform nil
    :accessor node-left)
   (%right
    :initarg :right
    :initform nil
    :accessor node-right)
   (%value
    :initarg :value
    :accessor node-value
    :initform 0)
   (%fillp
    :initarg :fillp
    :initform nil
    :accessor node-fillp)
   (%leafp
    :initarg :leafp
    :initform nil
    :accessor node-leafp)))

(defgeneric walk (node next)
  (:method ((node node) next)
    (if (zerop next) (node-left node) (node-right node))))

(defgeneric set-next (node next other)
  (:method ((node node) next (other node))
    (if (zerop next)
	(setf (node-left node) other)
	(setf (node-right node) other))))

(defclass tree ()
  ((%root
    :initform (make-instance 'node)
    :accessor tree-root)))

(defgeneric fill-tree (tree depth path thing)
  (:method ((tree tree) (depth integer) (path integer) (value integer))
    (let ((current (tree-root tree)))
      (dotimes (i depth)
	(let* ((bit-pos (- (1- depth) i))
	       (bit (ldb (byte 1 bit-pos) path))
	       (next (walk current bit)))
	  (if (null next)
	      (progn (setf next (make-instance 'node))
		     (when (= i (1- depth))
		       (setf (node-value next) value)
		       (setf (node-leafp next) t))
		     (when (zerop path)
		       (setf (node-fillp next) t))
		     (set-next current bit next))
	      (when (node-leafp next)
		(error "Node is leaf, no other following")))
	  (setf current next)))))
  (:method ((tree tree) (depth integer) (path integer) (node node))
    (let ((current (tree-root tree)))
      (dotimes (i depth)
	(let* ((bit-pos (- (1- depth) i))
	       (bit (ldb (byte 1 bit-pos) path))
	       (next (walk current bit)))
	  (if (null next)
	      (progn
		(if (= i (1- depth))
		    (setf next node)
		    (setf next (make-instance 'node)))
		(when (zerop path)
		  (setf (node-fillp next) t))
		(set-next current bit next))
	      (when (node-leafp next)
		(error "Node is leaf, no other following")))
	  (setf current next))))))

(defparameter +value-eol+ -2000)
(defparameter +value-fill+ -1000)
(defparameter +value-passmode+ -3000)
(defparameter +value-hmode+ -4000)

(defparameter +eol+ (make-instance 'node :leafp t :value +value-eol+))

(defparameter +fill+
  (let ((node (make-instance 'node :value +value-fill+ :right +eol+)))
    (setf (node-left node) node)
    node))

(defparameter +eol-only-tree+
  (let ((tree (make-instance 'tree)))
    (fill-tree tree 12 0 +fill+)
    (fill-tree tree 12 1 +eol+)
    tree))

(defparameter +black-run-tree+
  (let ((tree (make-instance 'tree)))
    (dotimes (i (length +black-codes+))
      (dotimes (j (length (nth i +black-codes+)))
	#+(or)(format t "~%(~D,~D) = ~A / ~A" i j (nth j (nth i +black-codes+)) (nth j (nth i +black-run-lengths+)))
	(fill-tree tree
		   (+ i 2)
		   (nth j (nth i +black-codes+))
		   (nth j (nth i +black-run-lengths+)))))
    (fill-tree tree 12 0 +fill+)
    (fill-tree tree 12 1 +eol+)
    tree))

(defparameter +white-run-tree+
  (let ((tree (make-instance 'tree)))
    (dotimes (i (length +white-codes+))
      (dotimes (j (length (nth i +white-codes+)))
	#+(or)(format t "~%(~D,~D) = ~A / ~A"
		i
		j
		(nth j (nth i +white-codes+))
		(nth j (nth i +white-run-lengths+)))
	(fill-tree tree
		   (+ i 4)
		   (nth j (nth i +white-codes+))
		   (nth j (nth i +white-run-lengths+)))))
    (fill-tree tree 12 0 +fill+)
    (fill-tree tree 12 1 +eol+)
    tree))

(defparameter +code-tree+
  (let ((tree (make-instance 'tree)))
    (fill-tree tree 4 1 +value-passmode+)
    (fill-tree tree 3 1 +value-hmode+)
    (fill-tree tree 1 1 0)
    (fill-tree tree 3 3 1)
    (fill-tree tree 6 3 2)
    (fill-tree tree 7 3 3)
    (fill-tree tree 3 2 -1)
    (fill-tree tree 6 2 -2)
    (fill-tree tree 7 2 -3)
    tree))

;;; ccitt-fax-decode - parameters
;;; /K (integer, default 0)
;;;   < 0 Group 4, pure 2D
;;;   = 0 Group 3, pure 1D
;;;   > 0 Group 3, mixed 2D, etc.
;;; /EndOfLine (boolean, default false)
;;; /EncodedByteAlign (boolean, default false)
;;; /Columns (integer, default 1728)
;;; /Rows (integer, default 0)
;;; /EndOfBlock (boolean, default true)
;;; /BlackIs1 (boolean, default false)
;;; /DamagedRowsBeforeError (integer, default 0)

(defun invert-bitmap (octet-vector)
  (loop for i from 0 below (length octet-vector)
	do (setf (aref octet-vector i)
		 (logand (lognot (aref octet-vector i)) #xFF))))

(defclass fax-decoder ()
  ((%columns
    :initarg :columns
    :accessor image-width)
   (%rows
    :initarg :rows
    :accessor image-height)
   (%byte-aligned
    :initarg :byte-aligned
    :reader byte-aligned
    :initform t)
   (%g3-2d
    :initarg :g3-2d
    :reader g3-2d
    :initform nil)
   (%type
    :initarg :type
    :accessor compression-type
    :initform :t4)
   (%vector
    :initarg :vector
    :accessor input-vector)
   (%vector-index
    :initform 0
    :accessor input-vector-index)
   (%bit-index
    :initform 7
    :accessor bit-index)
   (%decoded-row
    :accessor decoded-row
    :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
   (%decoded-bit-index
    :initform 7
    :accessor decoded-bit-index)
   (%changes
    :initform '()
    :accessor current-row-changes)
   (%last-change
    :accessor last-change
    :initform 0)))

(defun make-fax-decoder (vector k columns rows)
  (make-instance 'fax-decoder :vector vector :columns columns :rows rows
			      :type (cond ((< k 0)
					   :t6)
					  ((> k 0)
					   :rle)
					  ((= 0 k)
					   :t4))))

(defun next-bit (decoder)
  (with-accessors ((vector-index input-vector-index)
		   (bit-index bit-index)
		   (vector input-vector))
      decoder
    (when (< bit-index 0)
      (setf bit-index 7)
      (incf vector-index))
    (if (>= vector-index (length vector))
	:eof
	(prog1 (ldb (byte 1 bit-index) (aref vector vector-index))
	  (decf bit-index)))))

(defun reset (decoder)
  (setf (bit-index decoder) 7)
  (incf (input-vector-index decoder)))

(defun reset-output (decoder)
  (setf (decoded-row decoder)
	(make-array 0 :element-type '(unsigned-byte 8) :adjustable t)
	(decoded-bit-index decoder)
	7))

(defun decode-run (decoder tree)
  (let ((total 0)
	(n (tree-root tree)))
    (loop
      (let ((bit (next-bit decoder)))
	(setf n (walk n bit))
	(when (null n)
	  (error "Unkown code in Huffman RLE stream"))
	(when (node-leafp n)
	  (incf total (node-value n))
	  (if (>= (node-value n) 64)
	      (setf n (tree-root tree))
	      (if (plusp (node-value n))
		  (return-from decode-run total)
		  (return-from decode-run (image-width decoder)))))))))

(defun decode1d (decoder)
  (with-accessors ((columns image-width)
		   (current-row-changes current-row-changes))
      decoder
    (setf current-row-changes '())
    (let ((index 0)
	  (white t))
      (loop while (< index columns)
	    do (incf index
		     (decode-run
		      decoder
		      (if white +white-run-tree+ +black-run-tree+)))
	       (push index current-row-changes)
	       (setf white (not white))))
    (setf current-row-changes (nreverse current-row-changes))))

(defun decode2d (decoder)
  (with-accessors ((columns image-width)
		   (current-row-changes current-row-changes))
      decoder
    (let ((reference-row-changes (copy-list current-row-changes)))
      (setf current-row-changes '())
      (let ((white t)
	    (index 0))
	(loop while (< index columns)
	      do (loop for n = (tree-root +code-tree+)
			 then (walk n (next-bit decoder))
		       until (null n)
		       do (case (node-value n)
			    (+value-hmode+
			     (incf index
				   (decode-run
				    decoder
				    (if white +white-run-tree+ +black-run-tree+)))
			     (push index current-row-changes)
			     (incf index
				   (decode-run
				    decoder
				    (if white +black-run-tree+ +white-run-tree+)))
			     (push index current-row-changes))
			    (+value-passmode+
			     (let ((el (1+ (get-next-change
					    decoder
					    reference-row-changes index white))))
			       (if (>= el (length reference-row-changes))
				   (setf index columns)
				   (setf index (nth el reference-row-changes)))))
			    (t
			     (let ((el (get-next-change
					decoder
					reference-row-changes index white)))
			       (if (or (>= el (length reference-row-changes))
				       (= -1 el))
				   (setf index (+ columns (node-value n)))
				   (setf index (+ (nth el reference-row-changes)
						  (node-value n))))
			       (push index current-row-changes)
			       (setf white (not white)))))))))
    (setf current-row-changes (nreverse current-row-changes))))

(defun get-next-change (decoder reference-row-changes a0 white)
  (let ((start (dpb (if white 0 1) (byte 1 0) (last-change decoder))))
    (when (> start 2)
      (decf start 2))
    (when (zerop a0)
      (return-from get-next-change start))
    (loop for i from start below (length reference-row-changes) by 2
	  when (< a0 (nth i reference-row-changes))
	    do (setf (last-change decoder) i)
	       (return-from get-next-change i))
    -1))

(defun decode-row-type2 (decoder)
  (when (byte-aligned decoder)
    (reset decoder))
  (decode1d decoder))

(defun decode-row-type4 (decoder)
  (when (byte-aligned decoder)
    (reset decoder))
  (loop named outer
	do (let ((start-node (tree-root +eol-only-tree+)))
	     (loop for n = (walk start-node (next-bit decoder))
		     then (walk n (next-bit decoder))
		   until (null n)
		   when (node-leafp n)
		     do (return-from outer))))
  (if (or (not (g3-2d decoder))
	  (not (zerop (next-bit decoder))))
      (decode1d decoder)
      (decode2d decoder)))

(defun decode-row-type6 (decoder)
  (when (byte-aligned decoder)
    (reset decoder))
  (decode2d decoder))

(defun write-bits (decoder color count)
  (with-accessors ((row decoded-row)
		   (bit-index decoded-bit-index))
      decoder
    (let* ((prefix (min count (1+ bit-index)))
	   (bytes (floor (- count prefix) 8))
	   (suffix (rem (- count prefix) 8)))
      (assert (= count (+ prefix (* bytes 8) suffix)))
      (let ((base-byte (if (zerop color) #x00 #xFF)))
	(if (zerop (length row))
	    (vector-push-extend
	     (dpb (ldb (byte prefix 0) base-byte)
		  (byte prefix (- bit-index (1- prefix)))
		  base-byte)
	     row)
	    (dpb (ldb (byte prefix 0) base-byte)
		 (byte prefix (- bit-index (1- prefix)))
		 (aref row (1- (length row)))))
	(dotimes (i bytes)
	  (vector-push-extend base-byte row))
	(unless (zerop suffix)
	  (vector-push-extend
	   (dpb (ldb (byte suffix 0) base-byte)
		(byte suffix (- 7 suffix))
		0)
	   row))
	(if (not (zerop suffix))
	    (setf bit-index (- 7 suffix))
	    (if (zerop bytes)
		(decf bit-index prefix)
		(setf bit-index 7)))
	(when (minusp bit-index)
	  (setf bit-index 7))))))

(defun decode-row (decoder)
  (ecase (compression-type decoder)
    (:rle (decode-row-type2 decoder))
    (:t4 (decode-row-type4 decoder))
    (:t6 (decode-row-type6 decoder)))
  (print (current-row-changes decoder))
  (setf (last-change decoder) 0)
  (loop for color in '#1=(1 0 . #1#)
	for prev = 0 then next
	for next in (current-row-changes decoder)
	do (write-bits decoder color (- next prev))
	;;finally (format t "~%~D/~D" next (image-width decoder))
	))

(defun decode-rows (decoder)
  (loop repeat (image-height decoder)
	do (decode-row decoder)
	collecting (decoded-row decoder)
	do (reset-output decoder)))

(named-readtables:in-readtable pdf::syntax)

(defparameter *test-output* nil)

(defun test ()
  (let* ((doc (pdf::read-pdf-file "/mnt/c/Users/David/Downloads/AIM-801.pdf"))
	 (page (aref (pdf::document-pages doc) 0))
	 (image (pdf::get-page-xobject page #"Im0"))
	 (content (pdf::pdf-stream-content image))
	 (decoder (make-fax-decoder content -1 5120 6624)))
    (setf *test-output* (decode-rows decoder))
    (values)))
