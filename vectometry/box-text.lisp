;;;; boxtext.lisp

(in-package #:vectometry)

(defparameter *horizontal-alignments*
  #(:before :left :center :right :after))

(defparameter *vertical-alignments*
  #(:below :bottom :middle :top :atop))

(defun rotate-alignment (horizontal vertical rotation)
  (let ((h (position horizontal *horizontal-alignments*))
        (v (position vertical *vertical-alignments*)))
    (flet ((invert (i)
             (- (length *horizontal-alignments*) i 1)))
      (unless h
        (error "Invalid horizontal alignment ~S" horizontal))
      (unless v
        (error "Invalid vertical alignment ~S" vertical))
      (ecase rotation
        (:none)
        (:right
         (psetf h (invert v) v h))
        (:left
         (psetf v (invert h) h v))
        (:invert
         (psetf h (invert h) v (invert v))))
      (values (aref *horizontal-alignments* h)
              (aref *vertical-alignments* v)))))

(defun draw-box-text (box text &key size loader
                                 (horizontal :left) (vertical :bottom)
                                 (rotation :none))
  (let ((stringbox (string-bounding-box text size loader))
        (x (xmin box))
        (y (ymin box))
        (center (centerpoint box)))
    (flet ((handle-rotation (point degrees h v)
             (with-graphics-state
               (translate point)
               (rotate-degrees degrees)
               (let ((box* (if (= degrees 180)
                               box
                               (transpose box))))
                 (setf box* (displace box* (neg (minpoint box))))
                 (return-from draw-box-text
                   (draw-box-text box* text :size size :loader loader
                            :horizontal h :vertical v
                            :rotation :none))))))
      (ecase rotation
        (:none)
        (:left
         (multiple-value-bind (h v)
             (rotate-alignment horizontal vertical rotation)
           (handle-rotation (bottom-right box) 90 h v)))
        (:right
         (multiple-value-bind (h v)
             (rotate-alignment horizontal vertical rotation)
           (handle-rotation (top-left box) -90 h v)))
        (:invert
         (multiple-value-bind (h v)
             (rotate-alignment horizontal vertical rotation)
           (handle-rotation (maxpoint box) 180 h v))))
      (ecase horizontal
        (:before (setf x (- (xmin box) (width stringbox))))
        (:left)
        (:center (setf x (- (x center) (/ (width stringbox) 2))))
        (:right (setf x (- (xmax box) (xmax stringbox))))
        (:after (setf x (xmax box))))
      (ecase vertical
        (:atop (setf y (ymax box)))
        (:top (setf y (- (ymax box) size)))
        (:middle (setf y (- (y center) (/ size 2))))
        (:bottom)
        (:below (setf y (- (ymin box) size))))
      (let ((origin (point x y)))
        (draw-string origin text)))))

#+nil
(defun testo (file)
  (with-box-canvas (box 0 0 800 800)
    (let ((tbox (box 50 50 700 500))
          (font (get-font "~/Documents/Marydale.ttf")))
      (set-font font 18)
      (set-stroke-color (hsv-color 1 1 1))
      (rectangle tbox)
      (stroke)
      (draw-box-text tbox "CENTER ATOP" :size 18 :loader font
                     :horizontal :center
                     :vertical :atop
                     :rotation :invert)
      (draw-box-text tbox "AFTER BELOW" :size 18 :loader font
                     :horizontal :after
                     :vertical :below)
      (save-png file))))

