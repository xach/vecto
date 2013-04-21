;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; $Id: user-drawing.lisp,v 1.21 2007/10/01 14:12:55 xach Exp $

(in-package #:vecto)

(defvar *graphics-state*)
(setf (documentation '*graphics-state* 'variable)
      "The currently active graphics state. Bound for the
      duration of WITH-GRAPICS-STATE.")

;;; Low-level path construction

(defun %move-to (state x y)
  (let ((path (paths:create-path :open-polyline)))
    (push (setf (path state) path) (paths state))
    (paths:path-reset path (paths:make-point x y))))

(defun %line-to (state x y)
  (paths:path-extend (path state) (paths:make-straight-line)
                     (paths:make-point x y)))

(defun %curve-to (state cx1 cy1 cx2 cy2 x y)
  "Draw a cubic Bezier curve from the current point to (x,y)
through two control points."
  (let ((control-point-1 (paths:make-point cx1 cy1))
        (control-point-2 (paths:make-point cx2 cy2))
        (end-point (paths:make-point x y)))
    (paths:path-extend (path state)
                       (paths:make-bezier-curve (list control-point-1
                                                      control-point-2))
                       end-point)))

(defun %quadratic-to (state cx cy x y)
  "Draw a quadratic Bezier curve from the current point to (x,y)
through one control point."
  (paths:path-extend (path state)
                     (paths:make-bezier-curve (list (paths:make-point cx cy)))
                     (paths:make-point x y)))

(defun draw-arc-curves (curves)
  (destructuring-bind (((startx . starty) &rest ignored-curve)
                       &rest ignored-curves)
      curves
    (declare (ignore ignored-curve ignored-curves))
    (if (path *graphics-state*)
        (line-to startx starty)
        (move-to startx starty)))
  (loop for ((x1 . y1)
             (cx1 . cy1)
             (cx2 . cy2)
             (x2 . y2)) in curves
        do (curve-to cx1 cy1 cx2 cy2 x2 y2)))

(defun %close-subpath (state)
  (setf (paths::path-type (path state)) :closed-polyline))

;;; Clipping path

(defun %end-path-no-op (state)
  (after-painting state))

(defun %clip-path (state)
  (call-after-painting state
                       (make-clipping-path-function state :nonzero-winding)))

(defun %even-odd-clip-path (state)
  (call-after-painting state
                       (make-clipping-path-function state :even-odd)))

;;; Image drawing

(defun %draw-image (dest-image-data dest-image-width source-image-data 
                    source-image-width x y &key override-r override-g override-b)
  "Combine, with alpha blending, the RGBA octet vector SOURCE-IMAGE-DATA into
the RGBA octet vector DEST-IMAGE-DATA at offset (X,Y). The red, green, and
blue channels in SOURCE-IMAGE-DATA may be overridden as if all (non-transparent) 
pixels have a particular value for the overriden channel."
  (declare (type octet-vector dest-image-data source-image-data))
  (let* ((src-row-length (* source-image-width 4))
         (dst-row-length (* dest-image-width 4))
         (src-vec-length (length source-image-data))
         (start-index (* (+ x (* (- (height *graphics-state*) 
                                    y 
                                    (/ src-vec-length src-row-length)) 
                                 dest-image-width))
                         4)))
    (declare (type vector-index start-index src-row-length 
                   dst-row-length src-vec-length))
    (do* ((i-dst start-index (+ i-dst 4))
          (line-start i-dst (if (>= (- i-dst line-start) src-row-length)
                                (setf i-dst (+ (- dst-row-length src-row-length) i-dst))
                                line-start))
          (i-src 0 (+ i-src 4)))
         ((<= src-vec-length i-src))
      (declare (type vector-index i-dst line-start i-src))
      (let ((a.fg-octet (aref source-image-data (+ i-src 3))))
        (cond 
          ;; opaque pixel, copy without blending
          ((= a.fg-octet #xFF)
           (setf (aref dest-image-data i-dst)
                 (or override-r (aref source-image-data i-src))
                 (aref dest-image-data (+ i-dst 1))
                 (or override-g (aref source-image-data (+ i-src 1)))
                 (aref dest-image-data (+ i-dst 2))
                 (or override-b (aref source-image-data (+ i-src 2)))
                 (aref dest-image-data (+ i-dst 3)) #xFF))
          ;; semi-transparent pixel, blend (skip if fully transparent)
          ((plusp a.fg-octet)
           (let* ((a.fg (octet-float a.fg-octet))
                  (a.bg (octet-float (aref dest-image-data (+ i-dst 3))))
                  (a.bg*a.fg_inverse (* a.bg (- 1.0 a.fg)))
                  (a.new (+ a.fg a.bg*a.fg_inverse)))
             (declare (type float a.fg a.bg a.bg*a.fg_inverse a.new))
             (setf (aref dest-image-data (+ i-dst 3)) (float-octet a.new))
             (flet ((blend (fg bg)
                      (float-octet (/ 
                                    (+ (* (octet-float fg) a.fg) 
                                       (* (octet-float bg) a.bg*a.fg_inverse)) 
                                    a.new))))
               (if (zerop a.new)
                   (setf (aref dest-image-data i-dst) 0
                         (aref dest-image-data (+ i-dst 1)) 0
                         (aref dest-image-data (+ i-dst 2)) 0)
                   (setf (aref dest-image-data i-dst)
                         (blend (or override-r (aref source-image-data i-src)) 
                                (aref dest-image-data i-dst))
                         (aref dest-image-data (+ i-dst 1))
                         (blend (or override-g (aref source-image-data (+ i-src 1))) 
                                (aref dest-image-data (+ i-dst 1)))
                         (aref dest-image-data (+ i-dst 2))
                         (blend (or override-b (aref source-image-data (+ i-src 2))) 
                                (aref dest-image-data (+ i-dst 2)))))))))))))

;;; Text

(defun %get-font (state file)
  (find-font-loader state file))

(defun %set-font (state loader size)
  (let* ((scale (loader-font-scale size loader))
         (matrix (scaling-matrix scale scale)))
    (setf (font state)
          (make-instance 'font
                         :loader loader
                         :transform-matrix matrix
                         :size size))))

(defun %string-paths (state x y string)
  (let ((font (font state)))
    (unless font
      (error "No font currently set"))
    (string-primitive-paths x y string font
                            :character-spacing (character-spacing state))))

(defparameter *font-render-caches* (make-hash-table :test #'equal))
(defparameter *last-font* nil)
(defparameter *last-font-hash-key* nil)

(defun get-character-glyph-bitmap (character font)
  "Retrieve a glyph bitmap from the cache if it is present. Otherwise, render
it, add it to the cache, and return it."
  (let* ((font-key 
          ;; Use namestring of the font loader pathname as the hash
          ;; key. Namestring can be somewhat expensive to do on a
          ;; per-character basis depending on how it is implemented, hence we
          ;; cache it under *last-font-hash-key*
          (or (when (eq *last-font* font) *last-font-hash-key*) 
              (setf *last-font* font
                    *last-font-hash-key*
                    (namestring (zpb-ttf::input-stream (loader font))))))
         (font-cache-plist (gethash font-key *font-render-caches*))
         (font-cache (getf font-cache-plist (size font))))
    (unless font-cache
      (setf font-cache (make-hash-table :size 128))
      (setf (getf font-cache-plist (size font)) font-cache)
      (setf (gethash font-key *font-render-caches*) font-cache-plist))
    (let ((cached-image (gethash character font-cache))) 
      (if cached-image 
          cached-image
          (setf (gethash character font-cache) 
                (render-character-glyph-bitmap character font))))))

(defun render-character-glyph-bitmap (character font)
  "Returns a zpng:png instance containing the rendered character. More
efficient representations are possible but 32-bit RGBA was chosen for
convenience."
  (let* ((loader (loader font))
         (font-size (size font))
         (scale-factor (loader-font-scale font-size loader))
         (glyph (zpb-ttf:find-glyph character loader))
         (bbox (bounding-box glyph))
         (ch-width (+ 2 (round (* scale-factor (- (xmax bbox) (xmin bbox))))))
         (ch-height (+ 2 (round (* scale-factor (- (ymax bbox) (ymin bbox))))))
         (ch-x-offset (1+ (round (* scale-factor (- (xmin bbox))))))
         (ch-y-offset (1+ (round (* scale-factor (- (ymin bbox))))))
         (ch-state (make-instance 'graphics-state
                                  :fill-color (make-instance 'rgba-color 
                                                             :red 0.0 :green 0.0 
                                                             :blue 0.0 :alpha 1.0)
                                  :fill-source (fill-source *graphics-state*)
                                  :font-loaders (font-loaders *graphics-state*)
                                  :font font
                                  :character-spacing (character-spacing *graphics-state*))))
    (state-image ch-state ch-width ch-height)
    (fill-image (image-data ch-state) 1.0 1.0 1.0 0.0)
    (%draw-string ch-state ch-x-offset ch-y-offset (string character))
    (image ch-state)))

(defun draw-string-fast (x y string)
  "Like DRAW-STRING, but caches glyph bitmaps to avoid re-rendering. Results
are similar to DRAW-STRING, but might not be pixel-for-pixel identical."
  (let* ((font (font *graphics-state*))
         (font-size (size font))
         (loader (loader font))
         (spacing (character-spacing *graphics-state*))
         prev-char-width
         ch
         (ch-width 0)
         ch-height
         x-offset
         y-offset
         max-height
         (glyphs (string-glyphs string loader))
         glyph
         (scale-factor (loader-font-scale font-size loader)))
    (setf max-height 
          (let ((string-bbox (string-bounding-box string font-size loader 
                                                  :character-spacing spacing)))
            (+ 2 (round (- (ymax string-bbox) (ymin string-bbox))))))
    (dotimes (i (length string))
      (setf ch (char string i))
      (setf glyph (pop glyphs))
      (setf prev-char-width ch-width)
      (let* ((bbox (bounding-box glyph))
             (ymin (ymin bbox))
             (xmin (xmin bbox)))
        (setf ch-width (+ 2 (round (* scale-factor (- (xmax bbox) (xmin bbox))))))
        (setf ch-height (+ 2 (round (* scale-factor (- (ymax bbox) ymin)))))
        (setf x-offset (round (* scale-factor xmin)))
        (setf y-offset (round (* scale-factor ymin))))
      (unless (and (<= ch-width 2) (<= ch-height 2))
        (let ((image (get-character-glyph-bitmap ch font))
              (override-color (fill-color *graphics-state*)))
          (%draw-image (image-data *graphics-state*) (width *graphics-state*) (zpng:image-data image) 
                       (zpng:width image) (+ x x-offset -1) (+ y y-offset -1)
                       :override-r (float-octet (red override-color))
                       :override-g (float-octet (green override-color))
                       :override-b (float-octet (blue override-color)))))
      (when glyphs
        (let* ((w (zpb-ttf:advance-width glyph))
               (k (zpb-ttf:kerning-offset glyph (first glyphs) loader))
               (offset (round (* scale-factor (+ w k)))))
          (incf x offset))))))

(defun %draw-string (state x y string)
  (draw-paths/state (%string-paths state x y string)
                    state))

(defun %draw-centered-string (state x y string)
  (let* ((font (font state))
         (bbox (string-bounding-box string (size font) (loader font)
                                    :character-spacing (character-spacing state)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (%draw-string state (- x width/2) y string)))

(defun string-paths (x y string)
  (setf (paths *graphics-state*)
        (append (paths *graphics-state*)
                (%string-paths *graphics-state* x y string)))
  (values))

(defun centered-string-paths (x y string)
  (let* ((font (font *graphics-state*))
         (bbox (string-bounding-box string (size font) (loader font)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (setf (paths *graphics-state*)
          (append (paths *graphics-state*)
                  (%string-paths *graphics-state* (- x width/2) y string)))
    (values)))


;;; Low-level transforms

(defun %translate (state tx ty)
  (apply-matrix state (translation-matrix tx ty)))

(defun %scale (state sx sy)
  (apply-matrix state (scaling-matrix sx sy)))

(defun %skew (state x y)
  (apply-matrix state (skewing-matrix x y)))

(defun %rotate (state radians)
  (apply-matrix state (rotation-matrix radians)))

;;; User-level commands

(defun move-to (x y)
  (%move-to *graphics-state* x y))

(defun line-to (x y)
  (%line-to *graphics-state* x y))

(defun curve-to (cx1 cy1 cx2 cy2 x y)
  (%curve-to *graphics-state* cx1 cy1 cx2 cy2 x y))

(defun quadratic-to (cx cy x y)
  (%quadratic-to *graphics-state* cx cy x y))

(defun arc (cx cy r theta1 theta2)
  (loop while (< theta2 theta1) do (incf theta2 (* 2 pi)))
  (let ((curves
         (approximate-elliptical-arc cx cy r r 0 theta1 theta2)))
    (draw-arc-curves curves)))

(defun arcn (cx cy r theta1 theta2)
  (loop while (< theta1 theta2) do (decf theta2 (* 2 pi)))
  (let ((curves (approximate-elliptical-arc cx cy r r 0 theta2 theta1)))
    (draw-arc-curves (nreverse (mapcar #'nreverse curves)))))

(defun close-subpath ()
  (%close-subpath *graphics-state*))

(defun end-path-no-op ()
  (%end-path-no-op *graphics-state*)
  (clear-paths *graphics-state*))

(defun clip-path ()
  (%clip-path *graphics-state*))

(defun even-odd-clip-path ()
  (%even-odd-clip-path *graphics-state*))

(defun get-font (file)
  (%get-font *graphics-state* file))

(defun set-font (font size)
  (%set-font *graphics-state* font size))

(defun set-character-spacing (spacing)
  (setf (character-spacing *graphics-state*) spacing))

(defun draw-image (x y image)
  (%draw-image (image-data *graphics-state*) (width *graphics-state*) 
               (zpng:image-data image) (zpng:width image) x y))

(defun draw-string (x y string)
  (%draw-string *graphics-state* x y string))

(defun draw-centered-string (x y string)
  (%draw-centered-string *graphics-state* x y string))

(defun set-dash-pattern (vector phase)
  (if (zerop (length vector))
      (setf (dash-vector *graphics-state*) nil
            (dash-phase *graphics-state*) nil)
      (setf (dash-vector *graphics-state*) vector
            (dash-phase *graphics-state*) phase)))

(defun set-line-cap (style)
  (assert (member style '(:butt :square :round)))
  (setf (cap-style *graphics-state*) style))

(defun set-line-join (style)
  (assert (member style '(:bevel :miter :round)))
  (setf (join-style *graphics-state*) (if (eql style :bevel) :none style)))

(defun set-line-width (width)
  (setf (line-width *graphics-state*) width))

(defun set-rgba-color (color r g b a)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) (clamp-range 0.0 a 1.0))
  color)

(defun set-rgb-color (color r g b)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) 1.0)
  color)

(defun set-rgb-stroke (r g b)
  (set-rgb-color (stroke-color *graphics-state*) r g b))

(defun set-rgba-stroke (r g b a)
  (set-rgba-color (stroke-color *graphics-state*) r g b a))

(defun set-rgb-fill (r g b)
  (clear-fill-source *graphics-state*)
  (set-rgb-color (fill-color *graphics-state*) r g b))

(defun set-rgba-fill (r g b a)
  (clear-fill-source *graphics-state*)
  (set-rgba-color (fill-color *graphics-state*) r g b a))

(defun stroke ()
  (draw-stroked-paths *graphics-state*)
  (clear-paths *graphics-state*))

(defun stroke-to-paths ()
  (let ((paths (state-stroke-paths *graphics-state*)))
    (clear-paths *graphics-state*)
    (setf (paths *graphics-state*) paths)
    (%close-subpath *graphics-state*)))
  
(defun fill-path ()
  (draw-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill ()
  (draw-even-odd-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun fill-and-stroke ()
  (draw-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill-and-stroke ()
  (draw-even-odd-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))


(defun clear-canvas ()
  (let ((color (fill-color *graphics-state*)))
    (fill-image (image-data *graphics-state*)
                (red color)
                (green color)
                (blue color)
                (alpha color))))

(defun translate (x y)
  (%translate *graphics-state* x y))

(defun scale (x y)
  (%scale *graphics-state* x y))

(defun skew (x y)
  (%skew *graphics-state* x y))

(defun rotate (radians)
  (%rotate *graphics-state* radians))

(defun rotate-degrees (degrees)
  (%rotate *graphics-state* (* (/ pi 180) degrees)))

(defparameter *write-png-function* nil
  "Optional PNG writing function for use by SAVE-PNG. If NIL, ZPNG's
  facilities will be used. If non-NIL, is a function compatible with the
  lambda list (FILE IMAGE-DATA WIDTH HEIGHT) that encodes a PNG image into the
  file named by FILE, a pathname designator. IMAGE-DATA must be an RGBA array
  of type OCTET-VECTOR containing uncompressed image data. WIDTH and HEIGHT
  are the width and height of the image in pixels, respectively.")

(defun save-png (file)
  (let ((image (image *graphics-state*))) 
    (if *write-png-function*
        (funcall *write-png-function* file (zpng::image-data image) 
                 (zpng::width image) (zpng::height image))
        (zpng:write-png image file))))

(defun save-png-stream (stream)
  (zpng:write-png-stream (image *graphics-state*) stream))

(defmacro with-canvas ((&key width height (close-font-loaders t)) &body body)
  `(let ((*graphics-state* (make-instance 'graphics-state)))
     (state-image *graphics-state* ,width ,height)
     (unwind-protect
          (progn
            ,@body)
       (clear-state *graphics-state* :close-font-loaders ,close-font-loaders))))

(defmacro with-graphics-state (&body body)
  `(let ((*graphics-state* (copy *graphics-state*)))
     ,@body))
