;;;; pdfreader/test

(defpackage #:com.splittist.pdfreader.test
  (:use #:cl #:com.splittist.pdfreader #:parachute)
  (:local-nicknames (#:pdf #:com.splittist.pdfreader))
  (:import-from #:com.splittist.pdfreader
		#:syntax

		;; utils
		#:octets=
		#:octets-latin1
		#:latin1-octets
		#:digit-octet-p
		#:with-input-from-octet-vector
		#:+identity-text-matrix+
		#:make-matrix
		#:make-translation-matrix
		#:m*

		;; object
		#:pdf-object-value
		#:*names*
		#:pdf-name
		#:intern-name
		#:nameql
		#:make-pdf-array
		#:get-array
		#:get-vector
		#:get-length

		;; lexer
		#:*pdf-stream*
		#:eat-char
		#:eat-chars
		#:skip-white-space-and-comments
		#:read-hex-digit
		#:read-name
		#:read-literal-string
		#:read-hexadecimal-string
		#:+true+
		#:+false+
		#:+null+
		#:read-keyword
		#:read-number-or-keyword
		#:read-array
		#:read-dictionary-properties
		#:read-dictionary-or-stream
		#:read-dictionary
		#:read-pdf-stream

		;; document
		#:find-startxref
		))
