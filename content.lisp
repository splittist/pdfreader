;;;; pdfreader

(in-package #:com.splittist.pdfreader)

;;;# content stream operators

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *content-stream-operator-table*
  '(("b" "Close, fill, and stroke path using nonzero winding number rule" 60 0 () op-close-fill-stroke)
    ("B" "Fill and stroke path using nonzero winding number rule" 60 0 () op-fill-stroke)
    ("b*" "Close, fill, and stroke path using even-odd rule" 60 0 () op-close-eo-fill-stroke)
    ("B*" "Fill and stroke path using even-odd rule" 60 0 () op-eo-fill-stroke)
    ("BDC" "Begin marked-content sequence with property list" 320 2 (name properties) op-begin-marked-content)
    ("BI" "Begin inline image object" 92 0 () op-begin-image)
    ("BMC" "Begin marked-content sequence" 320 1 (name) op-begin-marked-content)
    ("BT" "Begin text object" 107 0 () op-begin-text)
    ("BX" "Begin compatibility section" 32 0 () op-begin-ignore-undef) ; FIXME)
    ("c" "Append curved segment to path (three control points)" 59 6 (number number number number number number) op-curve-to)
    ("cm" "Concatenate matrix to current transformation matrix" 57 6 (number number number number number number) op-concat)
    ("CS" "Set color space for stroking operations" 74 1 (name) op-set-stroke-color-space)
    ("cs" "Set color space for nonstroking operations" 74 1 (name) op-set-fill-color-space)
    ("d" "Set line dash pattern" 57 2 (array number) op-set-dash)
    ("d0" "Set glyph width in Type 3 font" 113 2 (number number) op-set-char-width)
    ("d1" "Set glyph width and bounding box in Type 3 font" 113 6 (number number number number number number) op-set-cache-device)
    ("Do" "Invoke named XObject" 87 1 (name) op-x-object)
    ("DP" "Define marked-content point with property list" 320 2 (name properties) op-mark-point)
    ("EI" "End inline image object" 92 0 () op-end-image)
    ("EMC" "End marked-content sequence" 320 0 () op-end-marked-content)
    ("ET" "End text object" 107 0 () op-end-text)
    ("EX" "End compatibility section" 32 0 () op-end-ignore-undef) ; FIXME
    ("f" "Fill path using nonzero winding number rule" 60 0 () op-fill)
    ("F" "Fill path using nonzero winding number rule (obsolete)" 60 0 () op-fill)
    ("f*" "Fill path using even-odd rule" 60 0 () op-eo-fill)
    ("G" "Set gray level for stroking operations" 74 1 (number) op-set-stroke-gray)
    ("g" "Set gray level for nonstroking operations" 74 1 (number) op-set-fill-gray)
    ("gs" "Set parameters from graphics state parameter dictionary" 57 1 (name) op-set-ext-g-state) ; FIXME
    ("h" "Close subpath" 59 0 () op-close-path)
    ("i" "Set flatness tolerance" 57 1 (number) op-set-flat)
    ("ID" "Begin inline image data" 92 0 () op-image-data)
    ("j" "Set line join style" 57 1 (integer) op-set-line-join)
    ("J" "Set line cap style" 57 1 (integer) op-set-line-cap)
    ("K" "Set CMYK color for stroking operations" 74 4 (number number number number) op-set-stroke-cmyk-color)
    ("k" "Set CMYK color for nonstroking operations" 74 4 (number number number number) op-set-fill-cmyk-color)
    ("l" "Append straight line segment to path" 59 2 (number number) op-line-to)
    ("m" "Begin new subpath" 59 2 (number number) op-move-to)
    ("M" "Set miter limit" 57 1 (number) op-set-miter-limit)
    ("MP" "Define marked-content point" 320 1 (name) op-mark-point)
    ("n" "End path without filling or stroking" 60 0 () op-end-path)
    ("q" "Save graphics state" 57 0 () op-save)
    ("Q" "Restore graphics state" 57 0 () op-restore)
    ("re" "Append rectangle to path" 59 4 (number number number number) op-rectangle)
    ("RG" "Set RGB color for stroking operations" 74 3 (number number number) op-set-stroke-rgb-color)
    ("rg" "Set RGB color for nonstroking operations" 74 3 (number number number) op-set-fill-rgb-color)
    ("ri" "Set color rendering intent" 57 1 (name) op-set-rendering-intent)
    ("s" "Close and stroke path" 60 0 () op-close-stroke)
    ("S" "Stroke path" 60 0 () op-stroke)
    ("SC" "Set color for stroking operations" 74 nil nil op-set-stroke-color) ; 33 number
    ("sc" "Set color for nonstroking operations" 74 nil nil op-set-fill-color) ; 33 number
    ("SCN" "Set color for stroking operations (ICCBased and special colour spaces)" 74 nil nil op-set-stroke-color-n) ; 33 number
    ("scn" "Set color for nonstroking operations (ICCBased and special colour spaces)" 74 nil nil op-set-fill-color-n) ; 33 number
    ("sh" "Paint area defined by shading pattern" 77 1 (name) op-sh-fill)
    ("T*" "Move to start of next text line" 108 0 () op-text-next-line)
    ("Tc" "Set character spacing" nil 1 (number) op-set-char-spacing)
    ("Td" "Move text position" 108 2 (number number) op-text-move)
    ("TD" "Move text position and set leading" 108 2 (number number) op-text-move-set)
    ("Tf" "Set text font and size" nil 2 (name number) op-set-font)
    ("Tj" "Show text" 109 1 (string) op-show-text)
    ("TJ" "Show text, allowing individual glyph positioning" 109 1 (array) op-show-space-text)
    ("TL" "Set text leading" nil 1 (number) op-set-text-leading)
    ("Tm" "Set text matrix and text line matrix" 108 6 (number number number number number number) op-set-text-matrix)
    ("Tr" "Set text rendering mode" nil 1 (integer) op-set-text-render)
    ("Ts" "Set text rise" nil 1 (number) op-set-text-rise)
    ("Tw" "Set word spacing" nil 1 (number) op-set-word-spacing)
    ("Tz" "Set horizontal text scaling" nil 1 (number) op-set-horizontal-scaling)
    ("v" "Append curved segment to path (initial point replicated)" 59 4 (number number number number) op-curve-to-1)
    ("w" "Set line width" 57 1 (number) op-set-line-width)
    ("W" "Set clipping path using nonzero winding number rule" 61 0 () op-clip)
    ("W*" "Set clipping path using even-odd rule" 61 0 () op-eo-clip)
    ("y" "Append curved segment to path (final point replicated)" 59 4 (number number number number) op-curve-to-2)
    ("'" "Move to next line and show text" 109 1 (string) op-move-show-text)
    ("\"" "Set word and character spacing, move to next line, and show text" 109 3 (number number string) op-move-set-show-text))))

(defclass operator ()
  ((%name
    :initarg :name
    :reader operator-name)
   (%arg-count
    :initarg :arg-count
    :reader arg-count)
   (%arg-types
    :initarg :arg-types
    :reader arg-types)
   (%function
    :initarg :function
    :reader operator-function)))

(defmethod print-object ((object operator) stream)
  (if *print-pretty*
      (princ (operator-name object) stream)
      (print-unreadable-object (object stream :type t :identity t)
	(princ (operator-name object) stream))))

(defclass unknown-operator (operator)
  ()
  (:default-initargs :function (lambda () (error "Unknown operator"))))

;;;## Operand types

(defun is-pdf-integer (thing)
  (and (typep thing 'pdf-number)
       (integerp (pdf-object-value thing))))

(deftype pdf-integer ()
  '(satisfies is-pdf-integer))

(deftype pdf-properties ()
  '(or pdf-name pdf-dictionary))

(defparameter *content-stream-types*
  '((number . pdf-number)
    (name . pdf-name)
    (array . pdf-array)
    (string . pdf-string)
    (properties . pdf-properties)
    (integer . pdf-integer)))

;; FIXME sc SC scn SCN
(defgeneric check-operands (operator operands)
  (:method ((operator operator) operands)
    (when (find (operator-name operator) '("sc" "SC" "scn" "SCN") :test #'string=)
      (return-from check-operands t))
    (unless (= (arg-count operator) (length operands))
      (error "Wrong number of operands for ~A. Wanted ~D, got ~D."
	     (operator-name operator) (arg-count operator) (length operands)))
    (loop for operand in operands
	  for type in (arg-types operator)
	  for pdf-type = (serapeum:assocdr type *content-stream-types*)
	  unless (typep operand pdf-type)
	    do (error "Wrong operand type for ~A. Wanted ~A, got ~A."
		      (operator-name operator) type operand))))

(defparameter *content-stream-operators*
  (let ((ht (make-hash-table :test 'equal :size 73)))
    (dolist (entry *content-stream-operator-table* ht)
      (destructuring-bind (name comment table count types function) entry
	(declare (ignore comment table))
	(setf (gethash name ht)
	      (make-instance 'operator :name name :arg-count count :arg-types types :function function))))))

;;;# Operator functions

;; b
(defgeneric op-close-fill-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; B
(defgeneric op-fill-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; b*
(defgeneric op-close-eo-fill-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; B*
(defgeneric op-eo-fill-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; BDC / BMC
(defgeneric op-begin-marked-content (operands output-device)
  (:method (operands output-device)
    nil))

;; BI
(defgeneric op-begin-image (operands output-device)
  (:method (operands output-device)
    nil))

;; BT
(defgeneric op-begin-text (operands output-device)
  (:method (operands output-device)
    nil))

;; BX
(defgeneric op-begin-ignore-undef (operands output-device)
  (:method (operands output-device)
    nil))

;; c
(defgeneric op-curve-to (operands output-device)
  (:method (operands output-device)
    nil))

;; cm
(defgeneric op-concat (operands output-device)
  (:method (operands output-device)
    nil))

;; CS
(defgeneric op-set-stroke-color-space (operands output-device)
  (:method (operands output-device)
    nil))

;; cs
(defgeneric op-set-fill-color-space (operands output-device)
  (:method (operands output-device)
    nil))

;; d
(defgeneric op-set-dash (operands output-device)
  (:method (operands output-device)
    nil))

;; d0
(defgeneric op-set-char-width (operands output-device)
  (:method (operands output-device)
    nil))

;; d1
(defgeneric op-set-cache-device (operands output-device)
  (:method (operands output-device)
    nil))

;; Do
(defgeneric op-x-object (operands output-device)
  (:method (operands output-device)
    nil))

;; DP
(defgeneric op-mark-point (operands output-device)
  (:method (operands output-device)
    nil))

;; EI
(defgeneric op-end-image (operands output-device)
  (:method (operands output-device)
    nil))

;; EMC
(defgeneric op-end-marked-content (operands output-device)
  (:method (operands output-device)
    nil))

;; ET
(defgeneric op-end-text (operands output-device)
  (:method (operands output-device)
    nil))

;; EX
(defgeneric op-end-ignore-undef (operands output-device)
  (:method (operands output-device)
    nil))

;; f / F
(defgeneric op-fill (operands output-device)
  (:method (operands output-device)
    nil))

;; f*
(defgeneric op-eo-fill (operands output-device)
  (:method (operands output-device)
    nil))

;; G
(defgeneric op-set-stroke-gray (operands output-device)
  (:method (operands output-device)
    nil))

;; g
(defgeneric op-set-fill-gray (operands output-device)
  (:method (operands output-device)
    nil))

;; gs
(defgeneric op-set-ext-g-state (operands output-device)
  (:method (operands output-device)
    nil))

;; h
(defgeneric op-close-path (operands output-device)
  (:method (operands output-device)
    nil))

;; i
(defgeneric op-set-flat (operands output-device)
  (:method (operands output-device)
    nil))

;; ID
(defgeneric op-image-data (operands output-device)
  (:method (operands output-device)
    nil))

;; j
(defgeneric op-set-line-join (operands output-device)
  (:method (operands output-device)
    nil))

;; J
(defgeneric op-set-line-cap (operands output-device)
  (:method (operands output-device)
    nil))

;; K
(defgeneric op-set-stroke-cymk-color (operands output-device)
  (:method (operands output-device)
    nil))

;; k
(defgeneric op-set-fill-cymk-color (operands output-device)
  (:method (operands output-device)
    nil))

;; l
(defgeneric op-line-to (operands output-device)
  (:method (operands output-device)
    nil))

;; m
(defgeneric op-move-to (operands output-device)
  (:method (operands output-device)
    nil))

;; M
(defgeneric op-set-miter-limit (operands output-device)
  (:method (operands output-device)
    nil))

;; MP
(defgeneric op-mark-point (operands output-device)
  (:method (operands output-device)
    nil))

;; n
(defgeneric op-end-path (operands output-device)
  (:method (operands output-device)
    nil))

;; q
(defgeneric op-save (operands output-device)
  (:method (operands output-device)
    nil))

;; Q
(defgeneric op-restore (operands output-device)
  (:method (operands output-device)
    nil))

;; re
(defgeneric op-rectangle (operands output-device)
  (:method (operands output-device)
    nil))

;; RG
(defgeneric op-set-stroke-rgb-color (operands output-device)
  (:method (operands output-device)
    nil))

;; rg
(defgeneric op-set-fill-rgb-color (operands output-device)
  (:method (operands output-device)
    nil))

;; ri
(defgeneric op-set-rendering-intent (operands output-device)
  (:method (operands output-device)
    nil))

;; s
(defgeneric op-close-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; S
(defgeneric op-stroke (operands output-device)
  (:method (operands output-device)
    nil))

;; SC
(defgeneric op-set-stroke-color (operands output-device)
  (:method (operands output-device)
    nil))

;; sc
(defgeneric op-set-fill-color (operands output-device)
  (:method (operands output-device)
    nil))

;; SCN
(defgeneric op-set-stroke-color-n (operands output-device)
  (:method (operands output-device)
    nil))

;; scn
(defgeneric op-set-fill-color-n (operands output-device)
  (:method (operands output-device)
    nil))

;; sh
(defgeneric op-sh-fill (operands output-device)
  (:method (operands output-device)
    nil))

;; T*
(defgeneric op-text-next-line (operands output-device)
  (:method (operands output-device)
    nil))

;; Tc
(defgeneric op-set-char-spacing (operands output-device)
  (:method (operands output-device)
    nil))

;; Td
(defgeneric op-text-move (operands output-device)
  (:method (operands output-device)
    nil))

;; TD
(defgeneric op-text-move-set (operands output-device)
  (:method (operands output-device)
    nil))

;; Tf
(defgeneric op-set-font (operands output-device)
  (:method (operands output-device)
    nil))

;; Tj
(defgeneric op-show-text (operands output-device)
  (:method (operands output-device)
    nil))

;; TJ
(defgeneric op-show-space-text (operands output-device)
  (:method (operands output-device)
    nil))

;; TL
(defgeneric op-set-text-leading (operands output-device)
  (:method (operands output-device)
    nil))

;; Tm
(defgeneric op-set-text-matrix (operands output-device)
  (:method (operands output-device)
    nil))

;; Tr
(defgeneric op-set-text-render (operands output-device)
  (:method (operands output-device)
    nil))

;; Ts
(defgeneric op-set-text-rise (operands output-device)
  (:method (operands output-device)
    nil))

;; Tw
(defgeneric op-set-word-spacing (operands output-device)
  (:method (operands output-device)
    nil))

;; Tz
(defgeneric op-set-horizontal-scaling (operands output-device)
  (:method (operands output-device)
    nil))

;; v
(defgeneric op-curve-to-1 (operands output-device)
  (:method (operands output-device)
    nil))

;; w
(defgeneric op-set-line-width (operands output-device)
  (:method (operands output-device)
    nil))

;; W
(defgeneric op-clip (operands output-device)
  (:method (operands output-device)
    nil))

;; W*
(defgeneric op-eo-clip (operands output-device)
  (:method (operands output-device)
    nil))

;; y
(defgeneric op-curve-to-2 (operands output-device)
  (:method (operands output-device)
    nil))

;; '
(defgeneric op-move-show-text (operands output-device)
  (:method (operands output-device)
    nil))

;; "
(defgeneric op-move-set-show-text (operands output-device)
  (:method (operands output-device)
    nil))

