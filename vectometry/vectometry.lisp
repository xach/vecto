;;;; vectometry.lisp

(in-package #:vectometry)

(defun move-to (p)
  (vecto:move-to (x p) (y p)))

(defun line-to (p)
  (vecto:line-to (x p) (y p)))

(defun curve-to (control1 control2 end)
  (vecto:curve-to (x control1) (y control1)
                  (x control2) (y control2)
                  (x end)      (y end)))

(defun quadratic-to (control end)
  (vecto:quadratic-to (x control) (y control)
                      (x end)     (y end)))

(defun draw-string (p string)
  (vecto:draw-string (x p) (y p) string))

(defun draw-centered-string (p string)
  (vecto:draw-centered-string (x p) (y p) string))

(defun string-paths (p string)
  (vecto:string-paths (x p) (y p) string))

(defun string-bounding-box (string size loader)
  (bbox-box (vecto:string-bounding-box string size loader)))

(defun arc (center radius theta1 theta2)
  (vecto:arc (x center) (y center) radius theta1 theta2))

(defun arcn (center radius theta1 theta2)
  (vecto:arcn (x center) (y center) radius theta1 theta2))

(defun rectangle (box)
  (vecto:rectangle (xmin box) (ymin box) (width box) (height box)))

(defun rounded-rectangle (box rx ry)
  (vecto:rounded-rectangle (xmin box) (ymin box)
                           (width box) (height box)
                           rx ry))

(defun centered-ellipse-path (center rx ry)
  (vecto:centered-ellipse-path (x center) (y center) rx ry))

(defun centered-circle-path (center radius)
  (vecto:centered-circle-path (x center) (y center) radius))

(defun translate (point)
  (vecto:translate (x point) (y point)))

(defmacro with-box-canvas (box &body body)
  (let ((box* (gensym "BOX")))
    `(let* ((,box* ,box))
       (with-canvas (:width (ceiling (width ,box*))
                            :height (ceiling (height ,box*)))
         (let ((p (neg (minpoint ,box*))))
           (translate (point (ceiling (x p))
                             (ceiling (y p)))))
         ,@body))))


(defgeneric top-left (object)
  (:method (object)
    (let ((box (bounding-box object)))
      (point (xmin box) (ymax box)))))

(defgeneric top-right (object)
  (:method (object)
    (maxpoint (bounding-box object))))

(defgeneric bottom-left (object)
  (:method (object)
    (minpoint (bounding-box object))))

(defgeneric bottom-right (object)
  (:method (object)
    (let ((box (bounding-box object)))
      (point (xmax box) (ymin box)))))

(defun set-gradient-fill (p1 c1 p2 c2
                          &key (extend-start t) (extend-end t)
                          (domain-function 'vecto:linear-domain))
  (vecto:set-gradient-fill (x p1) (y p1)
                           (red c1) (green c1) (blue c1) (alpha c1)
                           (x p2) (y p2)
                           (red c2) (green c2) (blue c2) (alpha c2)
                           :extend-start extend-start
                           :extend-end extend-end
                           :domain-function domain-function))


(defmethod bounding-box ((glyph zpb-ttf::glyph))
  (bbox-box (zpb-ttf:bounding-box glyph)))
