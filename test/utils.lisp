;;;; pdfreader/test

(in-package #:com.splittist.pdfreader.test)

(named-readtables:in-readtable syntax)

(define-test (#:all #:utils))

(define-test (#:utils #:octets=)
  (true (octets= #"abc" #"abc"))
  (false (octets= #"abc" #"abcd")))

(define-test (#:utils #:octets-latin1)
  (is string= "abc" (octets-latin1 #"abc")))

(define-test (#:utils #:latin1-octets)
  (is octets= #"abc" (latin1-octets "abc")))

(define-test (#:utils #:digit-octet-p)
  (is = 1 (digit-octet-p #!1)))




;;;# TEXT MATRICES

(define-test (#:utils #:make-matrix)
  (is equalp #(1 2 3 4 5 6 7 8 9)
      (make-matrix (list 1 2 3 4 5 6 7 8 9))))

(define-test (#:utils #:make-translation-matrix)
  (is equalp #(1 0 0 0 1 0 5 10 1)
      (make-translation-matrix 5 10)))

(define-test (#:utils #:m*)
  (is equalp #(1 2 3 4 5 6 7 8 9)
      (m* #(1 2 3 4 5 6 7 8 9) +identity-text-matrix+))
  (is equalp #(30 36 42 66 81 96 102 126 150)
      (m* #(1 2 3 4 5 6 7 8 9) #(1 2 3 4 5 6 7 8 9))))

;;;# PAPER SIZES


;;;# COLOURS


