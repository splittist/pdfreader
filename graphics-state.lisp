;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;;# GRAPHICS-STATE

(defclass graphics-state ()
  ((%ctm
    :initarg :ctm
    :accessor ctm) ; initform matrix transforming default user coords to device coords
   (%clipping-path
    :initarg :clipping-path
    :accessor clipping-path) ; initform boundary of entire portion of imageable page
   (%stroking-color-space
    :accessor stroking-color-space
    :initform (intern-name #"DeviceGray"))
   (%nonstroking-color-space
    :accessor nonstroking-color-space
    :initform (intern-name #"DeviceGray"))
   (%stroking-color
    :accessor stroking-color
    :initform 0.0)
   (%nonstroking-color
    :accessor nonstroking-color
    :initform 0.0)
   (%line-width
    :accessor line-width
    :initform 1.0)
   (%line-cap
    :accessor line-cap
    :initform 0) ; square
   (%line-join
    :accessor line-join
    :initform 0) ; mitered
   (%miter-limit
    :accessor miter-limit
    :initform 10.0)
   (%dash-pattern
    :accessor dash-pattern
    :initform (cons nil 0)) ; FIXME
   (%rendering-intent
    :accessor rendering-intent
    :initform (intern-name #"RelativeColorimetric"))
   (%stroke-adjustment
    :accessor stroke-adjustment
    :initform +false+)
   (%blend-mode
    :accessor blend-mode
    :initform (intern-name #"Normal"))
   (%soft-mask
    :accessor soft-mask
    :initform (intern-name #"None"))
   (%stroking-alpha-constant
    :accessor stroking-alpha-constant
    :initform 1.0)
   (%nonstroking-alpha-constant
    :accessor nonstroking-alpha-constant
    :initform 1.0)
   (%alpha-source
    :accessor alpha-source
    :initform +false+)
   ;; Device-Dependent Graphics State Parameters
   (%stroking-overprint
    :accessor stroking-overprint
    :initform +false+)
   (%nonstroking-overprint
    :accessor nonstroking-overprint
    :initform +false+)
   (%overprint-mode
    :accessor overprint-mode
    :initform 0)
   (%black-generation
    :accessor black-generation)
   (%undercolor-removal
    :accessor undercolor-removal)
   (%transfer
    :accessor transfer)
   (%halftone
    :accessor halftone)
   (%flatness
    :accessor flatness
    :initform 1.0)
   (%smoothness
    :accessor smoothness)
   ;; Text state parameters
   (%character-spacing
    :accessor character-spacing
    :initform 0)
   (%word-spacing
    :accessor word-spacing
    :initform 0)
   (%horizontal-scaling
    :accessor horizontal-scaling
    :initform 1.0) ; i.e. 100%
   (%leading
    :accessor leading
    :initform 0)
   (%text-font
    :accessor text-font)
   (%text-font-size
    :accessor text-font-size)
   (%text-rendering-mode
    :accessor text-rendering-mode
    :initform 0)
   (%text-rise
    :accessor text-rise
    :initform 0)
   (%text-knockout
    :accessor text-knockout)))

(defun copy-graphics-state (gs)
  (serapeum:lret ((new (make-instance 'graphics-state)))
    (loop for slot in (c2mop:class-slots (find-class 'graphics-state))
	  for slot-name = (c2mop:slot-definition-name slot)
	  when (slot-boundp gs slot-name)
	    do (setf (slot-value new slot-name)
		     (slot-value gs slot-name)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun set-dash-pattern (value gs)
  (let ((vector (get-vector (get-array value 0)))
	(phase (get-number (get-array value 1))))
    (setf (dash-pattern gs) (cons vector phase))))

(defun set-text-font-and-size (value gs)
  (let ((font (get-array value 0))
	(size (get-integer (get-array value 0))))
    (setf (text-font gs) font
	  (text-font-size gs) size)))
)

(defparameter *graphics-state-parameter-keys*
  `((#"LW" number  ,#'(setf line-width))
    (#"LC" integer ,#'(setf line-cap))
    (#"LJ" integer ,#'(setf line-join))
    (#"ML" number ,#'(setf miter-limit))
    (#"D" array ,#'set-dash-pattern) ;; [dashArray dashPhase]
    (#"RI" name ,#'(setf rendering-intent))
    (#"OP" boolean ,#'(setf stroking-overprint) ,#'(setf nonstroking-overprint))
    (#"op" boolean ,#'(setf nonstroking-overprint))
    (#"OPM" integer ,#'(setf overprint-mode))
    (#"Font" array ,#'set-text-font-and-size) ;; [font size], but font is indirect ref to font dictionary, not a name
    (#"BG" function ,#'(setf black-generation))
    (#"BG2" function-or-name ,#'(setf black-generation))
    (#"UCR" function ,#'(setf undercolor-removal))
    (#"UCR2" function-or-name ,#'(setf undercolor-removal))
    (#"TR" function-array-or-name ,#'(setf transfer))
    (#"TR2" function-array-or-name ,#'(setf transfer))
    (#"HT" dictionary-stream-or-name ,#'(setf halftone))
    (#"FL" number ,#'(setf flatness))
    (#"SM" number ,#'(setf smoothness))
    (#"SA" boolean ,#'(setf stroke-adjustment))
    (#"BM" name-or-array ,#'(setf blend-mode))
    (#"SMask" dictionary-or-name ,#'(setf soft-mask))
    (#"CA" number ,#'(setf stroking-alpha-constant) ,#'(setf nonstroking-alpha-constant))
    (#"ca" number ,#'(setf nonstroking-alpha-constant))
    (#"AIS" boolean ,#'(setf alpha-source))
    (#"TK" boolean ,#'(setf text-knockout))))

(defun set-gs-from-dictionary (gs dict)
  (dolist (entry *graphics-state-parameter-keys* gs)
    (alexandria:when-let ((value (get-dict (first entry) dict)))
      ;;check operand
      (funcall (third entry) value gs)
      (alexandria:when-let ((next (fourth entry)))
	(funcall next value gs)))))
