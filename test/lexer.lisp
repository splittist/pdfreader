;;;; pdfreader/test

(in-package #:com.splittist.pdfreader.test)

(named-readtables:in-readtable syntax)

(defmacro in ((vector) &body body)
  `(with-input-from-octet-vector (*pdf-stream* ,vector)
     ,@body))

;;;# LEXING

(define-test (#:all #:lexing))

(define-test (#:lexing #:eat-char)
  (in (#"abc")
    (finish (eat-char #!a))
    (finish (eat-char #!b))
    (fail (eat-char #!x))))

(define-test (#:lexing #:eat-chars)
  (in (#"abcdefghi")
    (finish (eat-chars #"abc"))
    (finish (eat-chars #"def"))
    (fail (eat-chars #"ghz"))))

(define-test (#:lexing #:skip-white-space-and-comments)
  (in (#"   abc")
    (is = #!a (skip-white-space-and-comments)))
  (in (#" 
  abc")
    (is = #!a (skip-white-space-and-comments)))
  (in (#"  % comment
  abc")
    (is = #!a (skip-white-space-and-comments)))
  (in (#"  ")
    (is eq :eof (skip-white-space-and-comments)))
  (in (#"  ")
    (fail (skip-white-space-and-comments t))))

(define-test (#:lexing #:read-hex-digit)
  (in (#"05ax")
    (is = 0 (read-hex-digit))
    (is = 5 (read-hex-digit))
    (is = #xa (read-hex-digit))
    (fail (read-hex-digit))))

(define-test (#:lexing #:read-name)
  :fix (*names*)
  (in (#"/Name1")
    (is nameql #"Name1" (read-name)))
  (in (#"/ASomewhatLongerName")
    (is nameql #"ASomewhatLongerName" (read-name)))
  (in (#"/A#42")
    (is nameql #"AB" (read-name))))

(defmacro is-string= (expected in)
  `(in (,in)
     (is octets= ,expected (pdf-object-value (read-literal-string)))))

(define-test (#:lexing #:read-literal-string)
  (is-string= #"foo" #"(foo)")
  (is-string= #"\\foo" #"(\\\\foo)")
  (is-string= #"Strings can contain newlines
and such" #"(Strings can contain newlines
and such)")
  (is-string= #"Strings can contain balanced parens () and special chars # & !"
	      #"(Strings can contain balanced parens () and special chars # & !)")
  (is-string= #"" #"()")
  (is-string= #"These two strings are the same" #"(These \\
two strings \\
are the same)")
  (is-string= #"This string contains `two octal characters'."
	      #"(This string contains \\140two octal characters\\47.)")
  (is-string= #"+" #"(\\053)")
  (is-string= #"+" #"(\\53)"))

(defmacro is-hexstring= (expected in)
  `(in (,in)
     (is octets= ,expected (pdf-object-value (read-hexadecimal-string)))))

(define-test (#:lexing #:read-hexadecimal-string)
  (is-hexstring= #"foobarbaz" #"<666F6F62617262617A>")
  (is-hexstring= #"foobarbap" #"<666F6F62617262617>")
  (is-hexstring= #"foobarbaz" #"<66 6F 6F 62 61 72 62 61 7A>")
  (in (#"<7G>")
    (fail (read-hexadecimal-string)))
  (in (#"666F6F62617262617A>")
    (is octets= #"foobarbaz" (pdf-object-value (read-hexadecimal-string t)))))

(define-test (#:lexing #:read-keyword)
  (in (#"true")
    (is eq +true+ (read-keyword)))
  (in (#"false")
    (is eq +false+ (read-keyword)))
  (in (#"null")
    (is eq +null+ (read-keyword))))

(defmacro is-properties= (expected in)
  `(in (,in)
     (is equalp
	 ,expected
	 (mapcar (lambda (entry)
		   (cons (pdf-object-value (car entry))
			 (pdf-object-value (cdr entry))))
		 (read-dictionary-properties)))))

(define-test (#:lexing #:read-dictionary-properties)
  (is-properties= '() #"< >>")
  (is-properties= '((#"Type" . #"Example")) #"</Type /Example>>")
  (is-properties= '((#"Type" . #"Example")) #"< /Type
   /Example     >>")
  (is-properties= '((#"IntegerItem" . 12)) #"< /IntegerItem 12 >>")
  (is-properties= '((#"StringItem" . #"a string")) #"< /StringItem (a string) >>")
  (is-properties= '((1 . 2) (3 . 4)) #"< 1 2 3 4 >>"))

(defparameter *stream.1* #"< /Length 10>>stream
1234567890
endstream")

(define-test (#:lexing #:read-dictionary-or-stream)
  (is-properties= '() #"< >>")
  (in (*stream.1*)
    (let ((result (read-dictionary-or-stream)))
      (is octets= #"1234567890" (pdf-stream-content result)))))
