;;;; pdfreader

(in-package #:com.splittist.pdfreader)

;;;# OUTPUT-DEVICE

(defclass output-device ()
  ())

;;;## RENDER-PAGE

(defgeneric render-page (page output-device))

