;;;; pdfreader

(defsystem #:pdfreader
  :description "A PDF reader"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license "GPL"
  :version "0.0.1"
  ;;:in-order-to ((asdf:test-op (asdf:test-op :cl-pdf-reader-test)))
  :depends-on (#:alexandria
	       #:serapeum
	       #:named-readtables
	       #:trivial-gray-streams
	       #:closer-mop 
	       #:semz.decompress
	       #:zpb-ttf
	       #:jqs-type1
	       #:vecto
	       #:colored
	       )
  :components ((:file "package")
	       (:file "readtable")
	       (:file "utils")
	       (:file "object")
	       (:file "lexer")
	       (:file "document")
	       (:file "filter")
	       (:file "encoding")
	       (:file "cmap")
	       (:file "page")
	       (:file "font")
	       (:file "graphics-state")
	       (:file "content")
	       (:file "device")
	       (:file "text-device")
	       (:file "vecto-fix")
	       (:file "vecto-device"))
  :in-order-to ((test-op (test-op #:pdfreader/test))))

(defsystem #:pdfreader/test
  :depends-on (#:pdfreader
	       #:parachute
	       )
  :components ((:module "test"
		:components ((:file "package")
			     (:file "all")
			     (:file "utils")
			     (:file "object")
			     (:file "lexer")
			     (:file "document"))
		))
  :perform (test-op (operation component)
		    (uiop:symbol-call '#:parachute '#:test '#:all)))
