;;;; pdfreader

(in-package #:com.splittist.pdfreader)

(named-readtables:in-readtable syntax)

(serapeum:define-do-macro do-pages ((page document &optional return) &body body)
  `(map nil (lambda (,page)
	      ,@body)
	(document-pages ,document)))


(defun get-page (document page-number)
  (ensure-object (aref (document-pages document) page-number)))

(defun get-page-parent (page)
  (get-dict #"Parent" (ensure-object page)))

(defun get-page-last-modified (page)
  (get-dict #"LastModified" page))

(defun get-page-resources (page)
  (alexandria:if-let ((result (get-dict #"Resources" (ensure-object page))))
    result
    (alexandria:if-let ((parent (get-page-parent page)))
      (get-page-resources parent)
      nil)))

(defun get-page-media-box (page)
 (alexandria:if-let ((result (get-dict #"MediaBox" (ensure-object page))))
    result
    (alexandria:if-let ((parent (get-page-parent page)))
      (get-page-media-box parent)
      nil)))

(defun get-page-crop-box (page)
  (or (alexandria:if-let ((result (get-dict #"CropBox" (ensure-object page))))
	result
	(alexandria:if-let ((parent (get-page-parent page)))
	  (get-page-crop-box parent)
	  nil))
      (get-page-media-box page)))

(defun get-page-bleed-box (page)
  (or (get-dict #"BleedBox" (ensure-object page))
      (get-page-crop-box page)))

(defun get-page-trim-box (page)
  (or (get-dict #"TrimBox" (ensure-object page))
      (get-page-crop-box page)))

(defun get-page-art-box (page)
  (or (get-dict #"ArtBox" (ensure-object page))
      (get-page-crop-box page)))

(defun get-page-box-color-info (page)
  (get-dict #"BoxColorInfo" (ensure-object page)))

(defun get-page-rotate (page)
  (or (alexandria:if-let ((result (get-dict #"Rotate" (ensure-object page))))
	result
	(alexandria:if-let ((parent (get-page-parent page)))
	  (get-page-rotate parent)
	  nil))
      0))

(defun get-page-group (page)
  (get-dict #"Group" (ensure-object page)))

(defun get-page-thumb (page)
  (get-dict #"Thumb" (ensure-object page)))

(defun get-page-beads (page)
  (get-dict #"B" (ensure-object page)))

(defun get-page-duration (page)
  (get-dict #"Dur" (ensure-object page)))

(defun get-page-transition (page)
  (get-dict #"Trans" (ensure-object page)))

(defun get-page-annotations (page)
  (get-dict #"Annots" (ensure-object page)))

(defun get-page-additional-actions (page)
  (get-dict #"AA" (ensure-object page)))

(defun get-page-metadata (page)
  (get-dict #"Metadata" (ensure-object page)))

(defun get-page-piece-info (page)
  (get-dict #"PieceInfo" (ensure-object page)))

(defun get-page-struct-parents (page)
  (get-dict #"StructParents" (ensure-object page)))

(defun get-page-id (page)
  (get-dict #"ID" (ensure-object page)))

(defun get-page-preferred-zoom (page)
  (get-dict #"PZ" (ensure-object page)))

(defun get-page-separation-info (page)
  (get-dict #"SeparationInfo" (ensure-object page)))

(defun get-page-tabs (page)
  (get-dict #"Tabs" (ensure-object page)))

(defun get-page-template-instantiated (page)
  (get-dict #"TemplateInstantiated" (ensure-object page)))

(defun get-page-pres-steps (page)
  (get-dict #"PresSteps" (ensure-object page)))

(defun get-page-user-unit (page)
  (or (get-dict #"UserUnit" (ensure-object page))
      1.0))

(defun get-page-viewports (page)
  (get-dict #"VP" (ensure-object page)))

(defun get-page-associated-files (page)
  (get-dict #"AF" (ensure-object page)))

(defun get-page-output-intents (page)
  (get-dict #"OutputIntents" (ensure-object page)))

(defun get-page-dpart (page)
  (get-dict #"DPart" (ensure-object page)))



(defun get-page-external-graphics-states (page)
  (ensure-object (get-dict #"ExtGState" (get-page-resources page))))

(defun get-page-external-graphics-state (page extgs-name)
  (ensure-object (get-dict extgs-name (get-page-external-graphics-states page))))

(defun get-page-fonts (page)
  (ensure-object (get-dict #"Font" (get-page-resources page))))

(defun get-page-font (page font-name)
  (ensure-object (get-dict font-name (get-page-fonts page))))
