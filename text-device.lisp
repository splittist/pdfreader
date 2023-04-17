;;;; pdfreader

(in-package #:com.splittist.pdfreader)

;;;# TEXT-OUTPUT-DEVICE

(defclass text-output-device (output-device)
  ((%page
    :accessor current-page)
   (%output
    :accessor text-output
    :initform '())
   (%gs-stack
    :initform '()
    :accessor gs-stack)
   (%current-gs
    :initform nil
    :accessor current-graphics-state
    :initarg :graphics-state)
   (%text-matrix
    :accessor text-matrix)
   (%prev-tm
    :accessor previous-matrix)
   (%text-line-matrix ; ???
    :accessor text-line-matrix)
   (%cmaps
    :initform '()
    :accessor cmaps)))

(defun make-text-output-device ()
  (make-instance 'text-output-device
		 :graphics-state (make-instance 'graphics-state :ctm (make-matrix))))

;;;## METHODS ON TEXT-OUTPUT-DEVICE

;; Tc
(defmethod op-set-char-spacing (operands (device text-output-device))
  (setf (character-spacing (current-graphics-state device)) (get-number (first operands))))

;; Tw
(defmethod op-set-word-spacing (operands (device text-output-device)) ; FIXME general?
  (setf (word-spacing (current-graphics-state device)) (get-number (first operands))))

;; Tz
(defmethod op-set-horizontal-scaling (operands (device text-output-device)) ; FIXME general?
  (setf (horizontal-scaling (current-graphics-state device)) (/ (get-number (first operands)) 100.0)))

;; TL
(defmethod op-set-text-leading (operands (device text-output-device)) ; FIXME general?
  (setf (leading (current-graphics-state device)) (get-number (first operands))))

;; Tf
(defmethod op-set-font (operands (device text-output-device))
  (let* ((font (get-page-font (current-page device) (first operands))))
    (setf (text-font (current-graphics-state device)) font
	  (text-font-size (current-graphics-state device)) (get-number (second operands)))))

;; Tr
(defmethod op-set-text-render (operands (device text-output-device))
  (setf (text-rendering-mode (current-graphics-state device)) (get-integer (first operands))))

;; Ts
(defmethod op-set-text-rise (operands (device text-output-device))
  (setf (text-rise (current-graphics-state device)) (get-number (first operands))))

;; BT
(defmethod op-begin-text (operands (device text-output-device))
  (setf (text-matrix device) (make-matrix)
	(text-line-matrix device) (make-matrix)
	(previous-matrix device) (make-matrix)))

;; ET
(defmethod op-end-text (operands (device text-output-device))
  (setf (text-matrix device) nil
	(text-line-matrix device) nil
	(previous-matrix device) nil))

;; Td
(defmethod op-text-move (operands (device text-output-device))
  (let* ((tx (get-number (first operands)))
	 (ty (get-number (second operands)))
	 (translation (make-translation-matrix tx ty))
	 (new-tm (m* (text-matrix device) translation)))
    (setf (text-matrix device) new-tm
	  (text-line-matrix device) new-tm)
    #+(or)(maybe-whitespace device)))

;; TD
(defmethod op-text-move-set (operands (device text-output-device))
  (let ((ty (get-number (second operands))))
    (op-set-text-leading (list (- ty)) device)
    (op-text-move operands device)))

;; Tm
(defmethod op-set-text-matrix (operands (device text-output-device)) ; FIXME general?
  (destructuring-bind (a b c d e f) (mapcar 'get-number operands)
    (let ((new (make-matrix (list a b 0 c d 0 e f 1))))
      (setf (text-matrix device) new
	    (text-line-matrix device) new)
      #+(or)(maybe-whitespace device))))

;; T*
(defmethod op-text-next-line (operands (device text-output-device))
  (op-text-move (list 0 (- (leading (current-graphics-state device)))) device))

;; Tj
(defmethod op-show-text (operands (device text-output-device))
  (let ((font (text-font (current-graphics-state device)))
	(tfs (text-font-size (current-graphics-state device)))
	(tc (character-spacing (current-graphics-state device)))
	(tw (word-spacing (current-graphics-state device)))
	(th (horizontal-scaling (current-graphics-state device))))
    (maybe-whitespace device)
    (loop for character-code across (get-string (first operands))
	  for w0 = (get-character-width font character-code)
	  for char = (code-char (character-code->unicode-value font character-code)) ; FIXME
	  do
	     (let* ((tx (* (+ (* w0 tfs) tc (if (char= #\Space char) tw 0)) th))
		    (old (text-matrix device))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1)) old)))
	       (setf (text-matrix device) new
		     (previous-matrix device) new))
	     (push char (text-output device)))))

;; '
(defmethod op-move-show-text (operands (device text-output-device))
  (op-text-next-line nil device)
  (op-show-text operands device))

;; "
(defmethod op-move-set-show-text (operands (device text-output-device))
  (op-set-word-spacing (list (get-number (first operands))) device)
  (op-set-char-spacing (list (get-number (second operands))) device)
  (op-move-show-text (list (third operands)) device))

;; TJ
(defmethod op-show-space-text (operands (device text-output-device))
  (loop for operand in (pdf-object-value (first operands))
	if (typep operand 'pdf-string)
	  do (op-show-text (list operand) device)
	if (typep operand 'pdf-number)
	  do (let* ((tx (* (text-font-size (current-graphics-state device))
			   (- (/ (get-number operand) 1000))
			   (horizontal-scaling (current-graphics-state device))))
		    (new (m* (make-matrix (list 1 0 0 0 1 0 tx 0 1)) (text-matrix device))))
	       (setf (text-matrix device) new)
	       #+(or)(maybe-horizontal-whitespace device))))
	       

;; q push gs on stack

;; Q try to pop gs from stack

;; cm
(defmethod op-concat (operands (device text-output-device)) ; FIXME general? / before?
  #+(or)(destructuring-bind (a b c d e f) operands
    (setf (ctm gs)
	  (m* (make-matrix (list a b 0 c d 0 e f 1))
	      (ctm gs)))))

(defun maybe-horizontal-whitespace (device)
  (let* ((tm (text-matrix device))
	 (m (m* tm (ctm (current-graphics-state device))))
	 (prev (previous-matrix device))
	 (dx (- (aref m 6) (aref prev 6))))
    (setf (previous-matrix device) m)
    (when (> dx 1)
	(unless (and (text-output device)
		     (white-space-p (first (text-output device))))
	  (push #\Space (text-output device))))))

;; FIXME - consecutive Tm's (for example) will not trigger new line even if collectively move by a line
(defun maybe-whitespace (device)
  (let* ((tm (text-matrix device))
	 (m (m* tm (ctm (current-graphics-state device))))
	 (prev (previous-matrix device))
	 (dx (- (aref m 6) (aref prev 6)))
	 (dy (- (aref m 7) (aref prev 7))))
    (setf (previous-matrix device) m)
    (if (< dy -1)
	(unless (and (text-output device)
		     (char= #\Newline (first (text-output device))))
	  (push #\Newline (text-output device)))
	(when (> dx 1)
	  (unless (and (text-output device)
		       (white-space-p (first (text-output device))))
	    (push #\Space (text-output device)))))))

(defun next-command ()
  (loop with operands = '()
	for obj = (read-object nil)
	until (eq :eof obj)
	do (if (and (typep obj 'pdf-keyword)
		    (gethash (octets-latin1 (pdf-object-value obj))
			     *content-stream-operators*))
	       (return (cons (gethash (octets-latin1 (pdf-object-value obj))
				      *content-stream-operators*)
			     (nreverse operands)))
	       (push obj operands))
	finally (unless (null operands)
		  (warn "Unused operands in contents stream: ~A" (nreverse operands)))
		(return nil)))

(named-readtables:in-readtable syntax)

(defun get-page-stream (page)
  (get-stream (ensure-object (get-dict #"Contents" (ensure-object page)))))

(defmethod render-page (page (device text-output-device))
  (setf (current-page device) page)
  (let ((*pdf-stream* (get-page-stream page)))
    (file-position *pdf-stream* 0)
    (loop for command = (next-command)
	  while command
	  do (let* ((operator (first command))
		    (opfunc (operator-function operator))
		    (operands (rest command)))
	       (check-operands operator operands)
	       (funcall opfunc operands device)))
    (concatenate 'string (nreverse (text-output device)))))
