;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(defun read-octet-string (s n c)
  (declare (ignore n))
  (let* ((string (funcall (get-macro-character #\") s #\"))
	 (ints (map '(vector (unsigned-byte 8)) #'char-code string)))
    (unless (every (lambda (int) (<= 0 int 255)) ints)
      (error 'reader-error :stream s))
    ints))

(defun read-octet-character (s n c)
  (let* ((char (funcall (get-dispatch-macro-character #\# #\\) s n c))
	 (code (char-code char)))
    (unless (<= 0 code 255)
      (error 'reader-error :stream s))
    code))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\" #'read-octet-string)
  (:dispatch-macro-char #\# #\! #'read-octet-character))
