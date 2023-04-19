;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

#+(or)(defparameter *standard-14-names*
  '("Times-Roman"
    "Helvetica"
    "Courier"
    "Symbol"

    "Times-Bold"
    "Helvetica-Bold"
    "Courier-Bold"
    "ZapfDingbats"

    "Times-Italic"
    "Helvetica-Oblique"
    "Courier-Oblique"

    "Times-BoldItalic"
    "Helvetica-BoldOblique"
    "Courier-BoldOblique"))

(serapeum:define-do-macro do-pages ((page document &optional return) &body body)
  `(map nil (lambda (,page)
	      ,@body)
	(document-pages ,document)))

(defun get-document-fonts (document)
  (let ((result '()))
    (do-pages (page document (mapcar 'ensure-object result))
      (let* ((page-fonts (get-page-fonts page))
	     (font-entries (mapcar 'cdr (pdf-object-value page-fonts))))
	(dolist (font font-entries)
	  (pushnew font result))))))

(defun get-page (document page-number)
  (ensure-object (aref (document-pages document) page-number)))

(defun get-page-parent (page)
  (get-dict #"Parent" (ensure-object page)))

(defun get-page-last-modified (page)
  (get-dict #"LastModified" page))

(defun get-page-resources (page)
  (alexandria:if-let ((result (get-dict #"Resources" (ensure-object page))))
    result
    (alexandria:if-let ((parent (get-page-parent page)))
      (get-page-resources parent)
      nil)))

(defun get-page-media-box (page)
  (alexandria:if-let ((result (get-dict #"MediaBox" (ensure-object page))))
    result
    (alexandria:if-let ((parent (get-page-parent page)))
      (get-page-media-box parent)
      nil)))

;; CropBox, BleedBox, TrimBox, ArtBox, BoxColorInfo

(defun get-page-rotate (page)
  (or (alexandria:if-let ((result (get-dict #"Rotate" (ensure-object page))))
	result
	(alexandria:if-let ((parent (get-page-parent page)))
	  (get-page-rotate parent)
	  nil))
      0))

(defun get-pgae-group (page)
  (get-dict #"Group" (ensure-object page)))

(defun get-page-thumb (page)
  (get-dict #"Thumb" (ensure-object page)))

(defun get-page-beads (page)
  (get-dict #"B" (ensure-object page)))

(defun get-page-duration (page)
  (get-dict #"Dur" (ensure-object page)))

(defun get-page-transition (page)
  (get-dict #"Trans" (ensure-object page)))

(defun get-page-annotations (page)
  (get-dict #"Annots" (ensure-object page)))

(defun get-page-additional-actions (page)
  (get-dict #"AA" (ensure-object page)))

(defun get-page-metadata (page)
  (get-dict #"Metadata" (ensure-object page)))

(defun get-page-piece-info (page)
  (get-dict #"PieceInfo" (ensure-object page)))

(defun get-page-struct-parents (page)
  (get-dict #"StructParents" (ensure-object page)))

(defun get-page-id (page)
  (get-dict #"ID" (ensure-object page)))

(defun get-page-preferred-zoom (page)
  (get-dict #"PZ" (ensure-object page)))

(defun get-page-separation-info (page)
  (get-dict #"SeparationInfo" (ensure-object page)))

(defun get-page-tabs (page)
  (get-dict #"Tabs" (ensure-object page)))

(defun get-page-template-instantiated (page)
  (get-dict #"TemplateInstantiated" (ensure-object page)))

(defun get-page-pres-steps (page)
  (get-dict #"PresSteps" (ensure-object page)))

(defun get-page-user-unit (page)
  (or (get-dict #"UserUnit" (ensure-object page))
      1.0))

(defun get-page-viewports (page)
  (get-dict #"VP" (ensure-object page)))

(defun get-page-associated-files (page)
  (get-dict #"AF" (ensure-object page)))

(defun get-page-output-intents (page)
  (get-dict #"OutputIntents" (ensure-object page)))

(defun get-page-dpart (page)
  (get-dict #"DPart" (ensure-object page)))



(defun get-page-external-graphics-states (page)
  (ensure-object (get-dict #"ExtGState" (get-page-resources page))))

(defun get-page-external-graphics-state (page extgs-name)
  (ensure-object (get-dict extgs-name (get-page-external-graphics-states page))))

(defun get-page-fonts (page)
  (ensure-object (get-dict #"Font" (get-page-resources page))))

(defun get-page-font (page font-name)
  (ensure-object (get-dict font-name (get-page-fonts page))))

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

(defun get-font-subtype (font)
  (get-dict #"Subtype" font))

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
  (alexandria:when-let ((fd (get-dict #"FontDescriptor" font)))
    (or (get-dict #"FontFile" fd) ; FIXME - only ask for possible ones?
	(get-dict #"FontFile2" fd)
	(get-dict #"FontFile3" fd))))

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
  (let ((first-char (get-integer (get-dict #"FirstChar" font)))
	(last-char (get-integer (get-dict #"LastChar" font)))
	(widths (ensure-object (get-dict #"Widths" font))))
    (assert (<= first-char character-code last-char))
    (get-integer (get-array widths (- character-code first-char)))))

;; single name can give multiple unicode code-points
(defgeneric unicode-from-map (map character-code))

(defmethod unicode-from-map ((cmap cmap) character-code)
  (vector (gethash character-code (cmap-entries cmap))))

(defmethod unicode-from-map ((encoding pdf-name) character-code)
  (gethash (character-code-name encoding character-code) +adobe-glyph-list+))

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
				(error "TBI"))))))))))))

