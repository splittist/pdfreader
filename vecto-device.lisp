;;;; pdf-reader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;;# VECTO-OUTPUT-DEVICE

(defclass vecto-output-device (output-device)
  ((%ppi
    :accessor pixels-per-inch
    :initarg :ppi
    :initform 300)
   (%current-page
    :accessor current-page)
   (%gs-stack
    :initform '()
    :accessor gs-stack)
   (%current-gs
    :accessor current-graphics-state
    :initform (make-instance 'graphics-state :ctm (make-matrix)))
   (%vecto-gs-stack
    :initform '()
    :accessor vecto-gs-stack)
   (%text-matrix
    :accessor text-matrix)
   (%text-line-matrix
    :accessor text-line-matrix)
   (%fonts
    :initform '()
    :accessor device-fonts)
   (%current-font
    :initform nil
    :accessor current-font-object)
   (%to-unicodes
    :initform (make-hash-table)
    :accessor to-unicode-maps)))

(defun make-vecto-output-device (&optional (pixels-per-inch 300))
  (serapeum:lret ((obj (make-instance 'vecto-output-device :ppi pixels-per-inch)))))

(defmethod render-page (page (device vecto-output-device))
  (let* ((user-unit (get-page-user-unit page))
	 (crop-box (get-page-crop-box page)) ;; FIXME crop box
	 (ppi (pixels-per-inch device))
	 (height (round (* (/ (get-number (get-array crop-box 3)) (* 72 user-unit)) ppi)))
	 (width (round (* (/ (get-number (get-array crop-box 2)) (* 72 user-unit)) ppi)))
	 (ctm (vector 25/6 0 0 0 25/6 0 0 0 1))) ;; FIXME
    (setf (current-page device) (ensure-object page)
	  (ctm (current-graphics-state device)) ctm ;; DEBUG
	  )
    (let ((*pdf-stream* (get-page-stream page)))
      (file-position *pdf-stream* 0)
      (vecto:with-canvas (:width width :height height)
	#+(or)(vecto::apply-matrix vecto::*graphics-state* (ctm-vecto ctm))
	(vecto:with-graphics-state
	  (vecto:set-rgb-fill 1 1 1)
	  (vecto:clear-canvas))
	(vecto:with-graphics-state
	  (vecto:scale 25/6 25/6)
	  (loop for command = (next-command)
		while command
		do 
		   (let* ((operator (first command))
			  (opfunc (operator-function operator))
			  (operands (rest command)))
		     (check-operands operator operands)
		     (funcall opfunc operands device)))
	  (vecto:save-png "c:/Users/David/Downloads/vecto.png"))
	#+(or)(dolist (entry (font-loaders device)) ;; DEBUG closes the underlying streams, which is annoying
		(ignore-errors (zpb-ttf:close-font-loader (cdr entry))))))))

(defun find-font-object (device name)
  (cdr (find name (device-fonts device) :test 'nameql :key 'car)))

(defun get-truetype-font-loader (device font-name)
  (alexandria:if-let ((font-entry (find-font-object device font-name)))
    font-entry
    (let ((stream (get-stream (get-font-file (get-page-font (current-page device) font-name)))))
      (file-position stream 0)
      (let ((font-loader (zpb-ttf:open-font-loader stream)))
	(push (cons font-name font-loader) (device-fonts device))
	font-loader))))

(defun get-cff-font (device font-name)
  (alexandria:if-let ((font-entry (find-font-object device font-name)))
    font-entry
    (let ((stream (get-stream (get-font-file (get-page-font (current-page device) font-name)))))
      (file-position stream 0)
      (let ((cff-font (type1:read-cff stream)))
	(push (cons font-name cff-font) (device-fonts device))
	cff-font))))

(defun pdf-vecto-color (color-space color)
  (serapeum:case-using 'nameql color-space
    (#"DeviceGray"
     (list color color color))
    (#"DeviceRGB"
     (list (colored:r color) (colored:g color) (colored:b color)))
    (#"DeviceCMYK"
     (let ((rgb (cmyk-rgb color)))
       (list (colored:r rgb) (colored:g rgb) (colored:b rgb))))
    (otherwise
     (warn "Dont' support color-space ~A" color-space))))

(defun ctm-vecto (ctm)
  (vector (aref ctm 0) (aref ctm 1) (aref ctm 3) (aref ctm 4) (aref ctm 6) (aref ctm 7)))

(defun update-vecto-from-gs (gs)  
  (with-accessors ((ctm ctm)
		   (stroking-color-space stroking-color-space)
		   (stroking-color stroking-color)
		   (nonstroking-color-space nonstroking-color-space)
		   (nonstroking-color nonstroking-color)
		   (line-width line-width)
		   (line-cap line-cap)
		   (line-join line-join)
		   (dash-pattern dash-pattern)
		   ;;(blend-mode blend-mode)
		   (horizontal-scaling horizontal-scaling)
		   (text-font text-font)
		   (text-font-size text-font-size))
      gs
    (apply 'vecto:set-rgb-fill (pdf-vecto-color nonstroking-color-space nonstroking-color))
    #+(or)(vecto:set-gradient-fill etc)
    (apply 'vecto:set-rgb-stroke (pdf-vecto-color stroking-color-space stroking-color))
    (vecto:set-line-cap (ecase line-cap (0 :butt) (1 :round) (2 :square)))
    (vecto:set-line-join (ecase line-join (0 :miter) (1 :round) (2 :bevel)))
    (vecto:set-line-width line-width)
    (vecto:set-dash-pattern (car dash-pattern) (cdr dash-pattern))
    #+(or)(vecto::apply-matrix vecto::*graphics-state* (ctm-vecto ctm))))

;;;# CONTENT OPERATIONS



;; q
(defmethod op-save (operands (device vecto-output-device))
  (push (copy-graphics-state (current-graphics-state device))
	(gs-stack device))
  (push (vecto::copy vecto::*graphics-state*)
	(vecto-gs-stack device)))

;; Q
(defmethod op-restore (operands (device vecto-output-device))
  (let ((prev (or (pop (gs-stack device))
		  (make-instance 'graphics-state :ctm (make-matrix)))))
    (setf (current-graphics-state device) prev)
    (setf vecto::*graphics-state* (pop (vecto-gs-stack device)))))

;; cm
(defmethod op-concat (operands (device vecto-output-device))
  (destructuring-bind (a b c d e f) (mapcar 'get-number operands)
    (let ((new (m* (make-matrix (list a b 0 c d 0 e f 1))
		   (ctm (current-graphics-state device)))))
      (setf (ctm (current-graphics-state device)) new)
      #+(or)(vecto::apply-matrix vecto::*graphics-state* (vector a b c d e f)))))

;; w
(defmethod op-set-line-width (operands (device vecto-output-device))
  (let ((width (get-number (first operands))))
    (setf (line-width (current-graphics-state device)) width)
    (vecto:set-line-width width))) ;; DEBUG * 3

;; J
(defmethod op-set-line-cap (operands (device vecto-output-device))
  (let ((style (get-integer (first operands))))
    (setf (line-cap (current-graphics-state device)) style)
    (vecto:set-line-cap (ecase style (0 :butt) (1 :round) (2 :square)))))

;; j
(defmethod op-set-line-join (operands (device vecto-output-device))
  (let ((style (get-integer (first operands))))
    (setf (line-join (current-graphics-state device)) style)
    (vecto:set-line-join (ecase style (0 :miter) (1 :round) (2 :bevel)))))

;; M
(defmethod op-set-miter-limit (operands (device vecto-output-device))
  (let ((limit (get-number (first operands))))
    (setf (miter-limit (current-graphics-state device)) limit)))

;; d
(defmethod op-set-dash (operands (device vecto-output-device))
  (let ((vector (get-vector (first operands)))
	(phase (get-number (second operands))))
    (setf (dash-pattern (current-graphics-state device))
	  (cons vector phase))
    (vecto:set-dash-pattern (map 'vector 'get-number vector) phase)))

;; ri
(defmethod op-set-rendering-intent (operands (device vecto-output-device))
  (let ((name (first operands)))
    (setf (rendering-intent (current-graphics-state device)) name)))

;; i
(defmethod op-set-flat (operands (device vecto-output-device))
  (let ((tolerance (get-number (first operands))))
    (setf (flatness (current-graphics-state device)) tolerance)))

;; gs
(defmethod op-set-ext-g-state (operands (device vecto-output-device))
  (let ((dict (get-page-external-graphics-state (current-page device) (first operands))))
    (set-gs-from-dictionary (current-graphics-state device) dict)
    (update-vecto-from-gs (current-graphics-state device))))

;; m
(defmethod op-move-to (operands (device vecto-output-device))
  (let ((x (get-number (first operands)))
	(y (get-number (second operands))))
    (vecto:move-to x y)))

;; l
(defmethod op-line-to (operands (device vecto-output-device))
  (let ((x (get-number (first operands)))
	(y (get-number (second operands))))
    (vecto:line-to x y)))

;; c
(defmethod op-curve-to (operands (device vecto-output-device))
  (let ((x1 (get-number (first operands)))
	(y1 (get-number (second operands)))
	(x2 (get-number (third operands)))
	(y2 (get-number (fourth operands)))
	(x3 (get-number (fifth operands)))
	(y3 (get-number (sixth operands))))
    (vecto:curve-to x1 y1 x2 y2 x3 y3)))

;; y
(defmethod op-curve-to-2 (operands (device vecto-output-device))
  (let ((x1 (get-number (first operands)))
	(y1 (get-number (second operands)))
	(x3 (get-number (third operands)))
	(y3 (get-number (fourth operands))))
    (vecto:curve-to x1 y1 x3 y3 x3 y3)))

;; h
(defmethod op-close-path (operands (device vecto-output-device))
  (vecto:close-subpath))

;; re
(defmethod op-rectangle (operands (device vecto-output-device))
  (let ((x (get-number (first operands)))
	(y (get-number (second operands)))
	(width (get-number (third operands)))
	(height (get-number (fourth operands))))
    (vecto:rectangle x y width height)))

;; S
(defmethod op-stroke (operands (device vecto-output-device))
  (vecto:stroke))

;; s
(defmethod op-close-stroke (operands (device vecto-output-device))
  (vecto:close-subpath)
  (vecto:stroke))

;; f / F
(defmethod op-fill (operands (device vecto-output-device))
  (vecto:fill-path))

;; f*
(defmethod op-eo-fill (operands (device vecto-output-device))
  (vecto:even-odd-fill))

;; B
(defmethod op-fill-stroke (operands (device vecto-output-device))
  (vecto:fill-and-stroke))

;; B*
(defmethod op-eo-fill-stroke (operands (device vecto-output-device))
  (vecto:even-odd-fill-and-stroke))

;; b
(defmethod op-close-fill-stroke (operands (device vecto-output-device))
  (vecto:close-subpath)
  (vecto:fill-and-stroke))

;; b*
(defmethod op-close-eo-fill-stroke (operands (device vecto-output-device))
  (vecto:close-subpath)
  (vecto:even-odd-fill-and-stroke))

;; n
(defmethod op-end-path (operands (device vecto-output-device))
  (vecto:end-path-no-op))

;; W
(defmethod op-clip (operands (device vecto-output-device))
  #+(or)(vecto:clip-path)) ;; FIXME (and below) clipping inside alternate content thingies

;; W*
(defmethod op-eo-clip (operands (device vecto-output-device))
  #+(or)(vecto:even-odd-clip-path))

;; CS
(defmethod op-set-stroke-color-space (operands (device vecto-output-device))
  (let ((name (ensure-object (first operands))))
    (serapeum:case-using 'nameql name
      (#"DeviceGray"
       (setf (stroking-color-space (current-graphics-state device)) name
	     (stroking-color (current-graphics-state device)) 0.0)
       (vecto:set-rgb-stroke 0 0 0))
      (#"DeviceRGB"
       (setf (stroking-color-space (current-graphics-state device)) name
	     (stroking-color (current-graphics-state device)) (colored:rgb 0 0 0))
       (vecto:set-rgb-stroke 0 0 0))
      (#"DeviceCMYK"
       (setf (stroking-color-space (current-graphics-state device)) name
	     (stroking-color (current-graphics-state device)) (colored:cmyk 0 0 0 0))
       (vecto:set-rgb-stroke 0 0 0))
      (otherwise
       (warn "Don't support color-space ~A" name)))))

;; cs
(defmethod op-set-fill-color-space (operands (device vecto-output-device))
  (let ((name (ensure-object (first operands))))
    (serapeum:case-using 'nameql name
      (#"DeviceGray"
       (setf (nonstroking-color-space (current-graphics-state device)) name
	     (nonstroking-color (current-graphics-state device)) 0.0)
       (vecto:set-rgb-fill 0 0 0))
      (#"DeviceRGB"
       (setf (nonstroking-color-space (current-graphics-state device)) name
	     (nonstroking-color (current-graphics-state device)) (colored:rgb 0 0 0))
       (vecto:set-rgb-fill 0 0 0))
      (#"DeviceCMYK"
       (setf (nonstroking-color-space (current-graphics-state device)) name
	     (nonstroking-color (current-graphics-state device)) (colored:cmyk 0 0 0 0))
       (vecto:set-rgb-fill 0 0 0))
      (otherwise
       (warn "Don't support color-space ~A" name)))))

;; SC
(defmethod op-set-stroke-color (operands (device vecto-output-device))
  (let ((color-space (stroking-color-space (current-graphics-state device))))
    (serapeum:case-using 'nameql color-space
      (#"DeviceGray"
       (assert (= 1 (length operands)))
       (let ((num (get-number (first operands))))
	 (setf (stroking-color (current-graphics-state device)) num)
	 (vecto:set-rgb-stroke num num num)))     
      (#"DeviceRGB"
       (assert (= 3 (length operands)))
       (let ((r (get-number (first operands)))
	     (g (get-number (second operands)))
	     (b (get-number (third operands))))
	 (setf (stroking-color (current-graphics-state device)) (colored:rgb r g b))
	 (vecto:set-rgb-stroke r g b)))
      (#"DeviceCMYK"
       (assert (= 4 (length operands)))
       (let* ((c (get-number (first operands)))
	      (m (get-number (second operands)))
	      (y (get-number (third operands)))
	      (k (get-number (fourth operands)))
	      (cmyk (colored:cmyk c m y k))
	      (rgb (cmyk-rgb cmyk)))
	 (setf (stroking-color (current-graphics-state device)) cmyk)
	 (vecto:set-rgb-stroke (colored:r rgb) (colored:g rgb) (colored:b rgb))))
      (otherwise
       (warn "Don't support color-space ~A" color-space)))))

;; sc
(defmethod op-set-fill-color (operands (device vecto-output-device))
  (let ((color-space (nonstroking-color-space (current-graphics-state device))))
    (serapeum:case-using 'nameql color-space
      (#"DeviceGray"
       (assert (= 1 (length operands)))
       (let ((num (get-number (first operands))))
	 (setf (nonstroking-color (current-graphics-state device)) num)
	 (vecto:set-rgb-fill num num num)))
      (#"DeviceRGB"
       (assert (= 3 (length operands)))
       (let ((r (get-number (first operands)))
	     (g (get-number (second operands)))
	     (b (get-number (third operands))))
	 (setf (nonstroking-color (current-graphics-state device)) (colored:rgb r g b))
	 (vecto:set-rgb-fill r g b)))
      (#"DeviceCMYK"
       (assert (= 4 (length operands)))
       (let* ((c (get-number (first operands)))
	      (m (get-number (second operands)))
	      (y (get-number (third operands)))
	      (k (get-number (fourth operands)))
	      (cmyk (colored:cmyk c m y k))
	      (rgb (cmyk-rgb cmyk)))
	 (setf (nonstroking-color (current-graphics-state device)) cmyk)
	 (vecto:set-rgb-fill (colored:r rgb) (colored:g rgb) (colored:b rgb))))
      (otherwise
       (warn "Don't support color-space ~A" color-space)))))

;; G
(defmethod op-set-stroke-gray (operands (device vecto-output-device))
  (let ((gray (get-number (first operands))))
    (assert (<= 0.0 gray 1.0))
    (setf (stroking-color-space (current-graphics-state device)) (intern-name #"DeviceGray")
	  (stroking-color (current-graphics-state device)) gray)
    (vecto:set-rgb-stroke gray gray gray)))

;; g
(defmethod op-set-fill-gray (operands (device vecto-output-device))
  (let ((gray (get-number (first operands))))
    (assert (<= 0.0 gray 1.0))
    (setf (nonstroking-color-space (current-graphics-state device)) (intern-name #"DeviceGray")
	  (nonstroking-color (current-graphics-state device)) gray)
    (vecto:set-rgb-fill gray gray gray)))

;; RG
(defmethod op-set-stroke-rgb-color (operands (device vecto-output-device))
  (let ((r (get-number (first operands)))
	(g (get-number (second operands)))
	(b (get-number (third operands))))
    (setf (stroking-color-space (current-graphics-state device)) (intern-name #"DeviceRGB")
	  (stroking-color (current-graphics-state device)) (colored:rgb r g b))
    (vecto:set-rgb-stroke r g b)))

;; rg
(defmethod op-set-fill-rgb-color (operands (device vecto-output-device))
  (let ((r (get-number (first operands)))
	(g (get-number (second operands)))
	(b (get-number (third operands))))
    (setf (nonstroking-color-space (current-graphics-state device)) (intern-name #"DeviceRGB")
	  (nonstroking-color (current-graphics-state device)) (colored:rgb r g b))
    (vecto:set-rgb-fill r g b)))

;; K
(defmethod op-set-stroke-cmyk-color (operands (device vecto-output-device))
  (let* ((c (get-number (first operands)))
	 (m (get-number (second operands)))
	 (y (get-number (third operands)))
	 (k (get-number (fourth operands)))
	 (cmyk (colored:cmyk c m y k))
	 (rgb (cmyk-rgb cmyk)))
    (setf (stroking-color-space (current-graphics-state device)) (intern-name #"DeviceCMYK")
	  (stroking-color (current-graphics-state device)) cmyk)
    (vecto:set-rgb-stroke (colored:r rgb) (colored:g rgb) (colored:b rgb))))

;; k
(defmethod op-set-fill-cmyk-color (operands (device vecto-output-device))
  (let* ((c (get-number (first operands)))
	 (m (get-number (second operands)))
	 (y (get-number (third operands)))
	 (k (get-number (fourth operands)))
	 (cmyk (colored:cmyk c m y k))
	 (rgb (cmyk-rgb cmyk)))
    (setf (nonstroking-color-space (current-graphics-state device)) (intern-name #"DeviceCMYK")
	  (nonstroking-color (current-graphics-state device)) cmyk)
    (vecto:set-rgb-fill (colored:r rgb) (colored:g rgb) (colored:b rgb))))

;; SCN / scn NOT IMPLEMENTED FIXME

;; sh (shading) NOT IMPLEMENTED FIXME

;; patterns and shading generally not implemented

;; Do paint specified XObject NOT IMPLEMENTED FIXME

;; images NOT IMPLEMENTED FIXME

;; BI
;; ID
;; EI

;; Tc
(defmethod op-set-char-spacing (operands (device vecto-output-device))
  (let ((spacing (get-number (first operands))))
    (setf (character-spacing (current-graphics-state device)) spacing)))

;; Tw
(defmethod op-set-word-spacing (operands (device vecto-output-device))
  (let ((spacing (get-number (first operands))))
    (setf (word-spacing (current-graphics-state device)) spacing)))

;; Tz
(defmethod op-set-horizontal-scaling (operands (device vecto-output-device))
  (let ((scale (/ (get-number (first operands)) 100.0)))
    (setf (horizontal-scaling (current-graphics-state device)) scale)
    (vecto:set-character-spacing scale)))

;; TL
(defmethod op-set-text-leading (operands (device vecto-output-device))
  (let ((leading (get-number (first operands))))
    (setf (leading (current-graphics-state device)) leading))) 

;; Tf
(defmethod op-set-font (operands (device vecto-output-device))
  (let* ((name (first operands))
	 (font-size (get-number (second operands)))
	 (font (get-page-font (current-page device) name))
	 (font-subtype (get-font-subtype font)))
    (cond ((nameql #"TrueType" font-subtype)
	   (let ((font-loader (get-truetype-font-loader device name)))
	     (setf (text-font (current-graphics-state device)) font
		   (text-font-size (current-graphics-state device)) font-size
		   (current-font-object device) font-loader)
	     (vecto:set-font font-loader font-size))) ;; DEBUG
	  ((nameql #"Type1" font-subtype)
	   (let ((cff-font (get-cff-font device name)))
	     (setf (text-font (current-graphics-state device)) font
		   (text-font-size (current-graphics-state device)) font-size
		   (current-font-object device) cff-font)))
	  ((nameql #"Type0" font-subtype)
	   (let ((descendant (get-font-descendant-font font)))
	     (cond ((nameql #"CIDFontType2" (get-font-subtype descendant))
		    (let ((font-loader (get-truetype-font-loader device name)))
		      (setf (text-font (current-graphics-state device)) font
			    (text-font-size (current-graphics-state device)) font-size
			    (current-font-object device) font-loader)
		      (vecto:set-font font-loader font-size)))
		   ((nameql #"CIDFontType0" (get-font-subtype descendant))
		    (let ((cff-font (get-cff-font device name)))
		      (setf (text-font (current-graphics-state device)) font
			    (text-font-size (current-graphics-state device)) font-size
			    (current-font-object device) cff-font)))
		   (t
		    (error "Unknown Descendant Font Type: ~A" (get-font-subtype descendant))))))
	  (t
	   (error "Unsupported font type: ~A" (octets-latin1 (pdf-object-value font-subtype)))))))

;; Tr
(defmethod op-set-text-render (operands (device vecto-output-device))
  (let ((mode (get-integer (first operands))))
    (setf (text-rendering-mode (current-graphics-state device)) mode)))

;; Ts
(defmethod op-set-text-rise (operands (device vecto-output-device))
  (let ((rise (get-number (first operands))))
    (setf (text-rise (current-graphics-state device)) rise)))

;; BT
(defmethod op-begin-text (operands (device vecto-output-device))
  (setf (text-matrix device) (make-matrix)
	(text-line-matrix device) (make-matrix)))

;; ET
(defmethod op-end-text (operands (device vecto-output-device))
  (setf (text-matrix device) nil
	(text-line-matrix device) nil))

;; Td
(defmethod op-text-move (operands (device vecto-output-device))
  (let* ((tx (get-number (first operands)))
	 (ty (get-number (second operands)))
	 (translation (make-translation-matrix tx ty))
	 (new-tm (m* translation (text-line-matrix device))))
    #+(or)(debug-line oldx oldy newx newy) ;; DEBUG
    (setf (text-matrix device) new-tm
	  (text-line-matrix device) (copy-seq new-tm))))

;; TD
(defmethod op-text-move-set (operands (device vecto-output-device))
  (let ((ty (get-number (second operands))))
    (op-set-text-leading (list (make-instance 'pdf-number :value (- ty))) device)
    (op-text-move operands device)))

;; Tm
(defmethod op-set-text-matrix (operands (device vecto-output-device))
  (destructuring-bind (a b c d e f) (mapcar 'get-number operands)
    (let ((new (make-matrix (list a b 0 c d 0 e f 1)))) ;; DEBUG
      (setf (text-matrix device) new
	    (text-line-matrix device) (copy-seq new)))))

;; T*
(defmethod op-text-next-line (operands (device vecto-output-device))
  (op-text-move (list (make-instance 'pdf-number :value 0)
		      (make-instance 'pdf-number :value (- (leading (current-graphics-state device)))))
		device))

;; '
(defmethod op-move-show-text (operands (device vecto-output-device))
  (op-text-next-line nil device)
  (op-show-text operands device))

;; "
(defmethod op-move-set-show-text (operands (device vecto-output-device))
  (op-set-word-spacing (list (first operands)) device)
  (op-set-char-spacing (list (second operands)) device)
  (op-move-show-text (list (third operands)) device))

;; TJ
(defmethod op-show-space-text (operands (device vecto-output-device))
  (loop for operand in (pdf-object-value (first operands))
	if (typep operand 'pdf-string)
	  do (op-show-text (list operand) device)
	if (typep operand 'pdf-number)
	  do (let* ((tx (* (text-font-size (current-graphics-state device))
			   (- (/ (get-number operand) 1000))
			   (horizontal-scaling (current-graphics-state device))))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1))
			     (text-matrix device))))
	       (setf (text-matrix device) new)
	       #+(or)(setf (text-line-matrix device) new)))) ;; XXX DEBUG XXX

;; Tj
(defmethod op-show-text (operands (device vecto-output-device))
  (let ((font (text-font (current-graphics-state device)))
	(tfs (text-font-size (current-graphics-state device)))
	(tc (character-spacing (current-graphics-state device)))
	(tw (word-spacing (current-graphics-state device)))
	(th (horizontal-scaling (current-graphics-state device))))
    ;; FIXME TODO - If the font is a CID font, then we may have to
    ;; extract bytes from the first operand by twos, or even by ones, twos, threes and fours
    ;; depending on the CID to GID map
    (loop with iterator = (make-text-iterator font (get-string (first operands)))
	  for character-code = (next-character-code iterator)
	  while character-code
	  for w0 = (/ (get-character-width font character-code) 1000) ;; DEBUG
	  ;;for chars = (character-code->unicode-value device character-code)
	  do
	     (let* ((tx (* (+ (* w0 tfs) tc (if (= #!Space character-code) tw 0)) th)) ;; FIXME ?
		    (old (text-matrix device))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1)) old)))
	       (show-character-code device (current-font-object device) character-code)
	       (setf (text-matrix device) new)
	       )))) ;; DEBUG

(defgeneric show-character-code (device font character-code))

(defmethod show-character-code ((device vecto-output-device) (font zpb-ttf::font-loader) character-code)
  (let* ((tm (text-matrix device))
	 (x (aref tm 6))
	 (y (aref tm 7)))
    ;; Note: while vecto (and zpb-ttf) treat(s) character-codes as characters, they really aren't,
    ;; except in the special (if common) case of certain encodings. The 'characters' are turned back
    ;; into character-codes for indexing into cmaps several layers later in zpb-ttf
    ;; DEBUG
    (if (simple-font-p (text-font (current-graphics-state device)))
	(vecto:draw-string x y (string (code-char character-code)))
	(vecto:draw-character x y character-code))))

(defmethod show-character-code ((device vecto-output-device) (font-object type1:cff-font) character-code)
  (let* ((font (text-font (current-graphics-state device)))
	 (encoding (get-type1-encoding font))
	 (character-name (character-code-name encoding character-code))
	 (glyph (type1:make-glyph font-object character-name)))
    (do-cff-glyph glyph device)))

(defmethod type1:move-to ((device vecto-output-device) x y)
  (vecto:move-to x y))

(defmethod type1:line-to ((device vecto-output-device) x y)
  (vecto:line-to x y))

(defmethod type1:curve-to ((device vecto-output-device) x1 y1 x2 y2 x3 y3)
  (vecto:curve-to x1 y1 x2 y2 x3 y3))

(defun do-cff-glyph (glyph device)
  (let* ((tm (text-matrix device))
	 (x (aref tm 6))
	 (y (aref tm 7))
	 (sx (aref tm 0))
	 (sy (aref tm 4)))
    (type1:with-glyph (glyph device)
      (vecto:with-graphics-state
	(vecto::apply-matrix vecto::*graphics-state* (vector (/ sx 1000) 0 0 (/ sy 1000)  x y))
	(map nil 'funcall (type1::glyph-code-vector glyph))
	(vecto:fill-path)))))
 
#+(or)(defun debug-line (x1 y1 x2 y2)
  (vecto:with-graphics-state
    (vecto:set-rgb-stroke 1.0 0.0 0.0)
    (vecto:set-line-width 1)
    (vecto:move-to x1 y1)
    (vecto:line-to x2 y2)
    (vecto:stroke)))
