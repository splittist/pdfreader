;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

;;;# OUTPUT-DEVICE

(defclass output-device ()
  ())

;;;## RENDER-PAGE

(defgeneric render-page (page output-device))

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

(defun get-page-stream (page)
  (let ((contents (get-dict #"Contents" (ensure-object page))))
    (etypecase contents
      (pdf-stream
       (get-stream contents))
      (pdf-array
       (let ((base-vectors (map 'list
				(lambda (entry)
				  (vector-stream-vector
				   (get-stream
				    (ensure-object entry))))
				(get-vector contents))))
	 ;; FIXME should really have a space between streams
	 (make-octet-vector-stream (apply 'concatenate 'octet-vector base-vectors)))))))

