;;;; pdfreader/test

(in-package #:com.splittist.pdfreader.test)

(named-readtables:in-readtable syntax)

(define-test (#:all #:document))

(define-test (#:document #:find-startxref)
  (pdf::with-input-from-octet-vector (*pdf-stream* #"   startxref 42 ")
    (is = 42 (find-startxref))))
