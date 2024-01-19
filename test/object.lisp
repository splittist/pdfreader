;;;; pdfreader/test

(in-package #:com.splittist.pdfreader.test)

(named-readtables:in-readtable syntax)

(define-test #:object)

(define-test (#:object #:intern-name)
  :fix (*names*)
  (is eq (intern-name #"foo") (intern-name #"foo"))
  (setf (gethash #"bar" *names*) (make-instance 'pdf::pdf-name :value #"bar")) ;; FIXME
  (is eq (intern-name #"bar") (intern-name #"bar")))

(define-test (#:object #:nameql)
  :fix (*names*)
  (true (nameql (intern-name #"foo") (intern-name #"foo")))
  (true (nameql #"foo" (intern-name #"foo")))
  (true (nameql (intern-name #"foo") #"foo"))
  (false (nameql (intern-name #"bar") #"foo")))

(define-test (#:object #:get-array)
  (let ((array (make-pdf-array (list 1 2 3 4 5 6))))
    (is = 1 (get-array array 0))
    (fail (get-array array 6))))

(define-test (#:object #:get-vector)
  (let ((array (make-pdf-array (list 1 2 3 4 5 6))))
    (is equalp #(1 2 3 4 5 6) (get-vector array)))
  (let ((array (make-pdf-array '())))
    (is equalp #() (get-vector array))))

(define-test (#:object #:get-length.array)
  (let ((array (make-pdf-array (list 1 2 3 4 5 6))))
    (is = 6 (get-length array))))
