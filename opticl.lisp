(in-package #:vecto)

(defclass opticl-graphics-state (graphics-state)
  ())

(defmethod image-data ((state opticl-graphics-state))
  (image state))

(defmethod state-image ((state opticl-graphics-state) width height)
  (setf (image state) (opticl:make-8-bit-rgba-image width height)
        (width state) width
        (height state) height
        (clipping-path state) (make-clipping-path width height))
  (apply-matrix state (translation-matrix 0 (- height))))

(defmethod copy ((state opticl-graphics-state))
  (make-instance 'opticl-graphics-state
                 :paths (paths state)
                 :path (path state)
                 :height (height state)
                 :width (width state)
                 :image (image state)
                 :stroke-color (copy (stroke-color state))
                 :line-width (line-width state)
                 :dash-vector (copy-seq (dash-vector state))
                 :dash-phase (dash-phase state)
                 :fill-color (copy (fill-color state))
                 :fill-source (fill-source state)
                 :join-style (join-style state)
                 :cap-style (cap-style state)
                 :transform-matrix (copy-seq (transform-matrix state))
                 :clipping-path (copy (clipping-path state))
                 :after-paint-fun (after-paint-fun state)
                 :font-loaders (font-loaders state)
                 :font (font state)
                 :character-spacing (character-spacing state)))

(defmethod save-to-png-file ((state opticl-graphics-state) file)
  (opticl:write-png-file file (image state)))

(defmethod save-to-png-stream ((state opticl-graphics-state) file)
  (opticl:write-png-stream stream (image state)))

(defmethod state-draw-function ((state opticl-graphics-state) fill-source fill-style)
  (make-opticl-draw-function (image-data state)
			     (clipping-path state)
			     (width state)
			     (height state)
			     fill-source
			     (ecase fill-style
			       (:even-odd #'even-odd-alpha)
			       (:nonzero-winding #'nonzero-winding-alpha))))

(defun make-opticl-draw-function (data clipping-path
				  width height
				  fill-source
				  alpha-fun)
  (if (emptyp clipping-path)
      (opticl-draw-function data width height fill-source alpha-fun)
      (opticl-draw-function/clipped data (clipping-data clipping-path)
                             width height
                             fill-source
                             alpha-fun)))

(defun opticl-draw-function (data width height fill-source alpha-fun)
  "From http://www.teamten.com/lawrence/graphics/premultiplication/"
  (declare (ignore height))
  (lambda (x y alpha)
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (funcall fill-source x y)
      (setf alpha (funcall alpha-fun alpha))
      (when (plusp alpha)
	(multiple-value-bind (r.bg g.bg b.bg a.bg)
	    (opticl:pixel data  y x)
	  (let* ((a.fg (imult alpha a.fg))
		 (gamma (prelerp a.fg a.bg a.bg)))
	    (flet ((blend (fg bg)
		     (let ((value (lerp (imult bg a.bg) fg a.fg)))
		       (float-octet (/ value gamma)))))
	      (if (zerop gamma)
		  (setf (opticl:pixel data y x)
			(values
			 r.bg
			 g.bg
			 b.bg
			 gamma))
		  (setf (opticl:pixel data y x)
			(values
			 (blend r.fg r.bg)
			 (blend g.fg g.bg)
			 (blend b.fg b.bg)
			 gamma))))))))))


(defun opticl-draw-function/clipped (data clip-data
				     width height
				     fill-source
				     alpha-fun)
  "Like DRAW-FUNCTION, but uses uses the clipping channel."
  (declare (ignore height))
  (lambda (x y alpha)
    (let* ((clip-index (+ x (* y width)))
           (clip (aref clip-data clip-index)))
      (setf alpha (imult clip (funcall alpha-fun alpha)))
      (when (plusp alpha)
        (multiple-value-bind (r.fg g.fg b.fg a.fg)
            (funcall fill-source x y)
	  (multiple-value-bind (r.bg g.bg b.bg a.bg)
	      (opticl:pixel data x y)
	    (let* ((a.fg (imult alpha a.fg))
		   (gamma (prelerp a.fg a.bg a.bg)))
	      (flet ((blend (fg bg)
		       (let ((value (lerp (imult bg a.bg) fg a.fg)))
			 (float-octet (/ value gamma)))))
		(if (zerop gamma)
		    (setf (opticl:pixel data y x)
			  (values
			   r.bg
			   g.bg
			   b.bg
			   gamma))
		    (setf (opticl:pixel data y x)
			  (values
			   (blend r.fg r.bg)
			   (blend g.fg g.bg)
			   (blend b.fg b.bg)
			   gamma)))))))))))
  


