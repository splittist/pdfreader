;;;; pdfreader

(in-package #:vecto)

(export 'draw-character)

(defun draw-character (x y character-code)
  (%draw-character *graphics-state* x y character-code))

(defun %draw-character (state x y character-code)
  (draw-paths/state (%character-paths state x y character-code)
		    state))

(defun %character-paths (state x y character-code)
  (let ((font (font state)))
    (unless font
      (error "No font currently set"))
    (character-primitive-paths x y character-code font)))

(defun character-primitive-paths (x y character-code font)
  (let* ((loader (loader font))
	 (glyph (zpb-ttf:index-glyph character-code loader))
	 (matrix (mult (transform-matrix font) (translation-matrix x y)))
	 (paths '()))
    (let ((glyph-paths (glyph-paths glyph))
	  (fun (make-transform-function matrix)))
      (dolist (path glyph-paths)
	(push (transform-path path fun) paths)))
    paths))
