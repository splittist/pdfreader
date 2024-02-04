;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

(defparameter *standard-14*
  '(("Times-Roman" "TimesNewRomanPSMT" "TimesNewRoman" "TimesNewRomanPS")
    ("Times-Bold" "TimesNewRomanPS-BoldMT" "TimesNewRoman,Bold" "TimesNewRomanPS-Bold" "TimesNewRoman-Bold")
    ("Times-Italic" "TimesNewRomanPS-ItalicMT" "TimesNewRoman,Italic" "TimesNewRomanPS-Italic" "TimesNewRoman-Italic")
    ("Times-BoldItalic" "TimesNewRomanPS-BoldItalicMT" "TimesNewRoman,BoldItalic" "TimesNewRomanPS-BoldItalic" "TimesNewRoman-BoldItalic")
    
    ("Helvetica" "ArialMT" "Arial")
    ("Helvetica-Bold" "Arial-BoldMT" "Arial,Bold" "Arial-Bold" "Helvetica,Bold") 
    ("Helvetica-Oblique" "Arial-ItalicMT" "Arial,Italic" "Arial-Italic" "Helvetica,Italic" "Helvetica-Italic")
    ("Helvetica-BoldOblique" "Arial-BoldItalicMT" "Arial,BoldItalic" "Arial-BoldItalic" "Helvetica,BoldItalic" "Helvetica-BoldItalic")
    
    ("Courier" "CourierNew" "CourierNewPSMT")
    ("Courier-Bold" "CourierNew,Bold" "Courier,Bold" "CourierNewPS-BoldMT" "CourierNew-Bold")
    ("Courier-Oblique" "CourierNew,Italic" "Courier,Italic" "CourierNewPS-ItalicMT" "CourierNew-Italic")
    ("Courier-BoldOblique" "CourierNew,BoldItalic" "Courier,BoldItalic" "CourierNewPS-BoldItalicMT" "CourierNew-BoldItalic")
    
    ("Symbol" "Symbol,Italic" "Symbol,Bold" "Symbol,BoldItalic" "SymbolMT" "SymbolMT,Italic" "SymbolMT,Bold" "SymbolMT,BoldItalic")
    
    ("ZapfDingbats")))

(defun standard-14-font-p (font-name)
  (let ((string-name (octets-latin1 (pdf-object-value font-name))))
    (dolist (list *standard-14*)
      (when (find string-name list :test 'string=) ;; FIXME string-equal better?
	(return (first list))))))

(defparameter *standard-14-font-equivalents*
  '(("Times-Roman" . "NimbusRoman-Regular")
    ("Times-Bold" . "NimbusRoman-Bold")
    ("Times-Italic" . "NimbusRoman-Italic")
    ("Times-BoldItalic" . "NimbusRoman-BoldItalic")

    ("Helvetica" . "NimbusSans-Regular")
    ("Helvetica-Bold". "NimbusSans-Bold")
    ("Helvetica-Oblique" . "NimbusSans-Italic")
    ("Helvetica-BoldOblique" . "NimbusSans-BoldItalic")

    ("Courier" . "NimbusMonoPS-Regular")
    ("Courier-Bold" . "NimbusMonoPS-Bold")
    ("Courier-Oblique" . "NimbusMonoPS-Italic")
    ("Courier-BoldOblique" . "NimbusMonoPS-BoldItalic")

    ("Symbol" . "StandardSymbolsPS")

    ("ZapfDingbats" . "D050000L")))

(defparameter *standard-14-font-equivalent-path*
  #+(or)#P"/usr/share/fonts/type1/urw-base35/"
  #P"/mnt/c/Users/David/Downloads/urw-base35-fonts-20200910/fonts/")

(defun standard-14-font-equivalent (standard-name)
  (let* ((file-name (serapeum:assocdr standard-name
				      *standard-14-font-equivalents*
				      :test #'string=))
	 (path-name (merge-pathnames file-name *standard-14-font-equivalent-path*))
	 (ttf-name (merge-pathnames path-name #P"*.ttf"))
	 (otf-name (merge-pathnames path-name #P"*.otf"))
	 (t1-name (merge-pathnames path-name #P"*.t1")))
    (list (probe-file ttf-name)
	  (probe-file otf-name)
	  (probe-file t1-name))))

(defun get-document-fonts (document)
  (let ((result '()))
    (do-pages (page document (mapcar 'ensure-object result))
      (let* ((page-fonts (get-page-fonts page))
	     (font-entries (mapcar 'cdr (pdf-object-value page-fonts))))
	(dolist (font font-entries)
	  (pushnew font result))))))

;; FIXME move this somewhere else
(defmacro get-dict2 (outermostdict key1 key2)
  (alexandria:with-gensyms (result)
    `(alexandria:when-let ((,result (get-dict ,key1 ,outermostdict)))
       (get-dict ,key2 (ensure-object ,result)))))

(defmacro get-dict2-number (outermostdict key1 key2)
  (alexandria:with-gensyms (result)
    `(alexandria:when-let ((,result (get-dict2 ,outermostdict ,key1 ,key2)))
       (get-number ,result))))

(defun get-font-encoding (font)
  (get-dict #"Encoding" font))

(defun get-font-encoding-differences (font)
  (alexandria:when-let ((encoding (get-font-encoding font)))
    (when (typep encoding 'pdf-dictionary)
      (get-dict #"Differences" encoding))))

(defun get-encoding-differences (encoding-array)
  (loop with result = (make-array 256 :initial-element nil)
	with index = 0
	for item in (pdf-object-value encoding-array)
	if (typep item 'pdf-number)
	  do (setf index (get-number item))
	else if (typep item 'pdf-name)
	       do (setf (aref result index) item)
		  (incf index)
	else do (error "Uknown element in Differences array: ~A" item)
	finally (return result)))

(defun get-type1-encoding (font)
  (let ((encoding (get-font-encoding font)))
    (if (typep encoding 'pdf-name)
	(name-encoding encoding)
	(get-encoding-differences (get-dict #"Differences" encoding)))))

(defun get-font-descendant-fonts (font)
  (get-dict #"DescendantFonts" font))

(defun get-font-descendant-font (font)
  (alexandria:when-let ((descendants (get-font-descendant-fonts font)))
    (get-array descendants 0)))

(defun get-font-subtype (font)
  (get-dict #"Subtype" font))

(defun get-font-cid-to-gid-map (font)
  (get-dict #"CIDToGIDMap" font))

(defun get-font-cid-system-info (font)
  (get-dict #"CIDSystemInfo" font))

(defun get-font-cid-system-info-registry (font)
  (get-dict2 font #"CIDSystemInfo" #"Registry"))

(defun get-font-cid-system-info-ordering (font)
  (get-dict2 font #"CIDSystemInfo" #"Ordering"))

(defun get-font-cid-system-info-supplement (font)
  (get-dict2 font #"CIDSystemInfo" #"Supplement"))

(defun get-font-dw (font)
  (or (get-dict #"DW" font)
      1000))

(defun get-font-w (font)
  (get-dict #"W" font))

(defun get-font-w-dict (font)
  (let ((w-list (pdf-object-value (get-font-w font)))
	(dict (make-hash-table)))
    (loop for index below (length w-list)
	  for item = (nth index w-list)
	  for next = (nth (1+ index) w-list)
	  if (and (typep item 'pdf-number) (typep next 'pdf-array))
	    do (loop for key from (get-number item)
		     for value in (pdf-object-value next)
		     do (setf (gethash key dict) (get-number value)))
	       (incf index)
	  else
	    do (let ((width (get-number (nth (+ index 2) w-list))))
		 (loop for key from (get-number item) upto (get-number next)
		       do (setf (gethash key dict) width))
		 (incf index 2)))
    dict))

(defun get-font-dw2 (font)
  (get-dict #"DW2" font))

(defun get-font-w2 (font)
  (get-dict #"W2" font))

;; obsolete
#+(or)(defun get-font-name (font)
  (get-dict #"Name" font))

(defun get-font-base-font (font)
  (get-dict #"BaseFont" font))

(defun get-font-to-unicode-cmap (font)
  (ensure-object (get-dict #"ToUnicode" font)))

(defun get-font-descriptor (font)
  (ensure-object (get-dict #"FontDescriptor" font)))

(defun get-font-name (font)
  (get-dict2 font #"FontDescriptor" #"FontName"))

(defun get-font-subsetp (font)
  (alexandria:when-let ((name (get-font-name font)))
    (let* ((bytes (pdf-object-value name))
	   (plus-pos (position #!+ bytes)))
      (when (and plus-pos (= 6 plus-pos))
	(let ((possible-tag (subseq bytes 0 6)))
	  (when (every (lambda (char) (<= #!A char #!Z)) possible-tag)
	    (values possible-tag (subseq bytes (1+ plus-pos)))))))))

(defun get-font-family (font)
  (get-dict2 font #"FontDescriptor" #"FontFamily"))

(defun get-font-stretch (font)
  (get-dict2 font #"FontDescriptor" #"FontStretch"))

(defun get-font-weight (font)
  (alexandria:when-let ((number (get-dict2 font #"FontDesciptor" #"FontWeight")))
    (get-number number)))

(defun get-font-flags (font)
  (get-dict2 font #"FontDescriptor" #"Flags"))

(defun get-font-fixed-pitch (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 0 (get-integer flags))))

(defun get-font-serif (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 1 (get-integer flags))))

(defun get-font-symbolic (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 2 (get-integer flags))))

(defun get-font-script (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 3 (get-integer flags))))

(defun get-font-italic (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 6 (get-integer flags))))

(defun get-font-all-cap (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 16 (get-integer flags))))

(defun get-font-small-cap (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 17 (get-integer flags))))

(defun get-font-force-bold (font)
  (alexandria:when-let ((flags (get-font-flags font)))
    (logbitp 18 (get-integer flags))))

(defun dump-flags (font)
  (let ((flags (get-font-flags font)))
    (if (null flags)
	(format t "~%No flags.")
	(let ((flags (get-integer flags)))
	  (format t "~&~:[~;FixedPitch ~]~:[~;Serif ~]~:[~;Symbolic ~]~:[~;Script ~]~:[~;Italic ~]~
                       ~:[~;AllCap ~]~:[~;SmallCap ~]~:[~;ForceBold~]"
		  (logbitp 0 flags) (logbitp 1 flags) (logbitp 2 flags) (logbitp 3 flags) (logbitp 6 flags)
		  (logbitp 16 flags) (logbitp 17 flags) (logbitp 18 flags))))))

(defun get-font-bbox (font)
  (alexandria:when-let ((result (get-dict2 font #"FontDescriptor" #"FontBBox")))
    (vector (mapcar 'get-number (pdf-object-value result)))))

(defun get-font-italic-angle (font)
  (get-dict2-number font #"FontDescriptor" #"ItalicAngle"))

(defun get-font-ascent (font)
  (get-dict2-number font #"FontDescriptor" #"Ascent"))

(defun get-font-descent (font)
  (get-dict2-number font #"FontDescriptor" #"Descent"))

(defun get-font-leading (font)
  (or (get-dict2-number font #"FontDescriptor" #"Leading")
      0))

(defun get-font-cap-height (font)
  (get-dict2-number font #"FontDescriptor" #"CapHeight"))

(defun get-font-x-height (font)
  (or (get-dict2-number font #"FontDescriptor" #"XHeight")
      0))

(defun get-font-stem-v (font)
  (or (get-dict2-number font #"FontDescriptor" #"StemV")
      0))

(defun get-font-stem-h (font)
  (or (get-dict2-number font #"FontDescriptor" #"StemH")
      0))

(defun get-font-avg-width (font)
  (or (get-dict2-number font #"FontDescriptor" #"AvgWidth")
      0))

(defun get-font-max-width (font)
  (or (get-dict2-number font #"FontDescriptor" #"MaxWidth")
      0))

(defun get-font-missing-width (font)
  (or (get-dict2-number font #"FontDescriptor" #"MissingWidth")
      0))

(defun get-font-file (font)
  (if (simple-font-p font)
      (alexandria:when-let ((fd (get-dict #"FontDescriptor" font)))
	(or (get-dict #"FontFile" fd) ; FIXME - only ask for possible ones?
	    (get-dict #"FontFile2" fd)
	    (get-dict #"FontFile3" fd)))
      (get-font-file (get-font-descendant-font font))))

;; deprecated in PDF 2.0
(defun get-font-char-set (font)
  (get-dict2 font #"FontDescriptor" #"CharSet"))

(defun get-cid-style-dictionary (font)
  (get-dict2 font #"FontDescriptor" #"Style"))

(defun get-cid-fd-dictionary (font)
  (get-dict2 font #"FontDescriptor" #"FD"))

;; deprecated in PDF 2.0
(defun get-cid-set-stream (font)
  (get-dict2 font #"FontDescriptor" #"CIDSet"))

(defun get-font-stream-length1 (font-stream)
  (alexandria:when-let ((result (get-dict #"Length1" font-stream)))
    (get-integer result)))

(defun get-font-stream-length2 (font-stream)
  (alexandria:when-let ((result (get-dict #"Length2" font-stream)))
    (get-integer result)))

(defun get-font-stream-length3 (font-stream)
  (alexandria:when-let ((result (get-dict #"Length3" font-stream)))
    (get-integer result)))

(defun get-font-stream-subtype (font-stream)
  (get-dict #"Subtype" font-stream))

(defun get-character-width (font character-code)
  (if (simple-font-p font)
      (let ((first-char (get-integer (get-dict #"FirstChar" font)))
	    (last-char (get-integer (get-dict #"LastChar" font)))
	    (widths (ensure-object (get-dict #"Widths" font))))
	(assert (<= first-char character-code last-char))
	(get-integer (get-array widths (- character-code first-char))))
      (let* ((descendant (get-array (get-font-descendant-fonts font) 0))
	     (widths (get-font-w-dict descendant))
	     (default (get-number (get-font-dw descendant))))
	(gethash character-code widths default))))

;; single name can give multiple unicode code-points
(defgeneric unicode-from-map (map character-code))

(defmethod unicode-from-map ((cmap cmap) character-code)
  (vector (gethash character-code (cmap-entries cmap))))

(defmethod unicode-from-map ((encoding pdf-name) character-code)
  (gethash (character-code-name encoding character-code) +adobe-glyph-list+))

(defun simple-font-p (font)
  (not (nameql #"Type0" (get-font-subtype font))))

;; FIXME 9.10.2
(defun character-code->unicode-value (device character-code)
  (let* ((font (text-font (current-graphics-state device)))
	 (to-unicode-map (gethash font (to-unicode-maps device))))
    (if to-unicode-map
	(unicode-from-map to-unicode-map character-code)
	(let ((cmap-entry (get-font-to-unicode-cmap font)))
	  (if cmap-entry
	      (let ((cmap (parse-cmap (get-stream (get-font-to-unicode-cmap font)))))
		(setf (gethash font (to-unicode-maps device)) cmap)
		(unicode-from-map cmap character-code))
	      (let ((subtype (get-font-subtype font)))
		(cond ((nameql #"TrueType" subtype)
		       (let ((encoding (get-font-encoding font)))
			 (cond ((typep encoding 'pdf-name)
				(setf (gethash font (to-unicode-maps device)) encoding)
				(unicode-from-map encoding character-code))
			       ((typep encoding 'pdf-dictionary)
				(error "TrueType encoding dictionaries TBI"))
			       (t (error "Unknown TrueType Encoding: ~A" encoding)))))
		      ((or (nameql #"Type1" subtype) (nameql #"MMType1" subtype))
		       (let ((encoding (get-font-encoding font)))
			 (cond ((typep encoding 'pdf-name)
				(setf (gethash font (to-unicode-maps device)) encoding)
				(unicode-from-map encoding character-code))
			       ((typep encoding 'pdf-dictionary)
				(error "Type1 encoding dictionaries TBI"))
			       (t (error "Unknown Type1 Encoding: ~A" encoding)))))
		      ;; FIXME ??
		      ((nameql #"Type0" subtype)
		       (let* ((encoding (get-font-encoding font))
			      (descendant-font (get-array (get-font-descendant-fonts font) 0))
			      (descendant-subtype (get-font-subtype descendant-font))
			      (descendant-encoding (get-font-cid-to-gid-map descendant-font)))
			 (if (and (nameql #"CIDFontType2" descendant-subtype)
				  (typep encoding 'pdf-name)
				  (or (nameql #"Identity-H" encoding) (nameql #"Identity-V" encoding))
				  (typep descendant-encoding 'pdf-name)
				  (nameql #"Identity" descendant-encoding))
			     character-code
			     (error "Type0 font encoding (other than simplest) TBI"))))
		      ((nameql #"Type3" subtype)
		       (error "Type3 font encoding TBI"))
		      ((or (nameql #"CIDFontType0" subtype) (nameql #"CIDFontType2" subtype))
		       (error "CID Font encoding TBI"))
		      (t (error "Unknown Font SubType: ~A" subtype)))))))))

(defclass text-iterator ()
  ((%font
    :initarg :font
    :reader text-iterator-font)
   (%string
    :initarg :string
    :reader text-iterator-string)
   (%index
    :initform 0
    :accessor text-iterator-index)
   (%function
    :accessor text-iterator-function)))

(defun make-text-iterator (font string)
  (serapeum:lret ((iterator (make-instance 'text-iterator :font font :string string)))
    (if (simple-font-p font)
	(setf (text-iterator-function iterator) #'%next-byte)
	(let* ((encoding (get-font-encoding font))
	       (descendant-font (get-array (get-font-descendant-fonts font) 0))
	       (descendant-subtype (get-font-subtype descendant-font))
	       (descendant-encoding (get-font-cid-to-gid-map descendant-font)))
	  (if (and (nameql #"CIDFontType2" descendant-subtype)
		   (typep encoding 'pdf-name)
		   (or (nameql #"Identity-H" encoding) (nameql #"Identity-V" encoding))
		   (typep descendant-encoding 'pdf-name)
		   (nameql #"Identity" descendant-encoding))
	      (setf (text-iterator-function iterator) #'%next-two-bytes)
	      (error "Type0 font encoding (other than simplest) TBI"))))))

(defun %next-byte (text-iterator)
  (with-accessors ((index text-iterator-index)
		   (string text-iterator-string))
      text-iterator
    (if (< index (length string))
	(prog1 (aref string index)
	  (incf index))
	nil)))

(defun %next-two-bytes (text-iterator)
  (with-accessors ((index text-iterator-index)
		   (string text-iterator-string))
      text-iterator
    (if (< index (length string))
	(let ((first (aref string index))
	      (second (aref string (1+ index))))
	  (prog1 (+ (ash first 8) second)
	    (incf index 2)))
	nil)))

(defun next-character-code (text-iterator)
  (funcall (text-iterator-function text-iterator) text-iterator))
