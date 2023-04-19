;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;; Quick and Dirty Cmap parsing
;;; Pretend it's basically PDF tokens

(defclass cmap ()
  ((%dict
    :accessor cmap-dictionary
    :initform (make-hash-table :test 'equal))
   (%codespaces
    :accessor cmap-code-space-ranges
    :initform '())
   (%entries
    :accessor cmap-entries
    :initform (make-hash-table))))

(defun dump-cmap (cmap)
  (let ((dict-entries (alexandria:hash-table-alist (cmap-dictionary cmap)))
	(ranges (cmap-code-space-ranges cmap))
	(entries (sort (alexandria:hash-table-alist (cmap-entries cmap))
		       '<
		       :key 'car)))
    (print "Dictionary")
    (print dict-entries)
    (print "Ranges")
    (print ranges)
    (print "Entries")
    (print entries)))

(defun make-cmap ()
  (make-instance 'cmap))

(defun cmap-code-in-range-p (cmap character-code)
  (loop for (start . end) in (cmap-code-space-ranges cmap)
	when (<= start character-code end)
	  do (return t)
	finally (return nil)))

(defun add-cmap-code-space-range (cmap start end)
  (push (cons start end)
	(cmap-code-space-ranges cmap)))

(defun add-cmap-dictionary-entry (cmap key value)
  (setf (gethash key (cmap-dictionary cmap)) value))

(defun get-cmap-dictionary-entry (cmap key)
  (gethash key (cmap-dictionary cmap)))

(defun add-cmap-entry (cmap key value)
  (unless (cmap-code-in-range-p cmap key)
    (error "Cmap: bf-char out of range: ~D" key))
  (setf (gethash key (cmap-entries cmap)) value))

(defun get-cmap-entry (cmap key)
  (unless (cmap-code-in-range-p cmap key)
    (error "Cmap: character-code out of range: ~D" key))
  (gethash key (cmap-entries cmap)))

(defun keyword= (object octets)
  (and (typep object 'pdf-keyword)
       (octets= octets (pdf-object-value object))))

(defun pdf-string-hex (pdf-string)
  (loop for octet across (pdf-object-value pdf-string)
	with result = 0
	do (setf result (+ (ash result 4) octet))
	finally (return result)))

(defun expect-keyword (octets)
  (let ((obj (read-object)))
    (unless (keyword= obj octets)
      (warn "Parsing Cmap: Expected '~A' but got '~A'" (octets-latin1 octets) obj))))

(defun parse-cmap (stream)
  (let ((*pdf-stream* stream)
	(cmap (make-cmap))
	(stack '()))
    (file-position *pdf-stream* 0)
    (loop for obj = (read-object nil)
	  until (eq :eof obj)
	  do (serapeum:case-using 'keyword= obj
	       (#"begincmap"
		(setf stack '()))
	       (#"def"
		(let ((value (pop stack))
		      (key (pop stack)))
		  (add-cmap-dictionary-entry cmap key value)))
	       (#"begincodespacerange"
		(loop repeat (get-integer (pop stack))
		      do (let ((start (pdf-string-hex (read-object)))
			       (end (pdf-string-hex (read-object))))
			   (add-cmap-code-space-range cmap start end)))
		(expect-keyword #"endcodespacerange"))
	       (#"beginbfchar"
		(loop repeat (get-integer (pop stack))
		      do (let ((key (pdf-string-hex (read-object)))
			       (value (pdf-string-hex (read-object))))
			   (add-cmap-entry cmap key value)))
		(expect-keyword #"endbfchar"))
	       (#"beginbfrange"
		(loop repeat (get-integer (pop stack))
		      do (let ((start-key (pdf-string-hex (read-object)))
			       (end-key (pdf-string-hex (read-object)))
			       (start-value (pdf-string-hex (read-object))))
			   (loop for key from start-key upto end-key
				 for value from start-value
				 do (add-cmap-entry cmap key value))))
		(expect-keyword #"endbfrange"))
	       (#"endcmap"
		(return cmap))
	       (t
		(push obj stack))))))

