;;;; box.lisp

(in-package #:vecto-geometry)

(defclass box ()
  ((xmin
    :initarg :xmin
    :accessor xmin)
   (ymin
    :initarg :ymin
    :accessor ymin)
   (xmax
    :initarg :xmax
    :accessor xmax)
   (ymax
    :initarg :ymax
    :accessor ymax)))


(defmethod initialize-instance :before ((box box) &key
                                        xmin ymin xmax ymax)
  (assert (<= xmin xmax))
  (assert (<= ymin ymax)))

(defmethod xmin ((point point))
  (x point))

(defmethod xmax ((point point))
  (x point))

(defmethod ymin ((point point))
  (y point))

(defmethod ymax ((point point))
  (y point))

(defmethod print-object ((object box) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A,~A ~A,~A"
            (xmin object)
            (ymin object)
            (xmax object)
            (ymax object))))

(defun box (xmin ymin xmax ymax)
  (make-instance 'box
                 :xmin xmin
                 :ymin ymin
                 :xmax xmax
                 :ymax ymax))

(defun point-box (a b)
  "Creates the smallest box that contains the points A and B."
  (box (min (x a) (x b)) (min (y a) (y b))
       (max (x a) (x b)) (max (y a) (y b))))

(defun origin-box (point)
  "Creates a bounding box that includes both the origin and POINT."
  (point-box *origin* point))

(defun bbox-box (bbox)
  "Creates a box from the BBOX vector."
  (box (aref bbox 0)
       (aref bbox 1)
       (aref bbox 2)
       (aref bbox 3)))

(defgeneric minpoint (box)
  (:method ((box box))
    (point (xmin box) (ymin box))))

(defgeneric maxpoint (box)
  (:method ((box box))
    (point (xmax box) (ymax box))))

(defgeneric centerpoint (box)
  (:method ((box box))
    (midpoint (minpoint box) (maxpoint box))))

(defgeneric width (object)
  (:method (object)
    (- (xmax object) (xmin object))))

(defgeneric height (object)
  (:method (object)
    (- (ymax object) (ymin object))))

(defgeneric area (box)
  (:method ((box box))
    (* (width box) (height box))))

(defgeneric emptyp (box)
  (:method ((box box))
    ;; A little more efficient than (zerop (area box))
    (or (= (xmax box) (xmin box))
        (= (ymax box) (ymin box)))))

(defun contract (box amount)
  (let ((p (point amount amount)))
    (point-box (add (minpoint box) p)
               (sub (maxpoint box) p))))

(defun expand (box amount)
  (contract box (- amount)))

(defun %point-box-add (point box)
  (point-box (add (minpoint box) point)
             (add (maxpoint box) point)))

(defmethod add/2 ((point point) (box box))
  (%point-box-add point box))

(defmethod add/2 ((box box) (point point))
  (%point-box-add point box))

(defun %point-box-mul (point box)
  (point-box (mul (minpoint box) point)
             (mul (maxpoint box) point)))

(defmethod mul ((point point) (box box))
  (%point-box-mul point box))

(defmethod mul ((box box) (point point))
  (%point-box-mul point box))

(defmethod sub ((box box) (point point))
  (point-box (sub (minpoint box) point)
             (sub (maxpoint box) point)))

(defmethod div ((box box) (point point))
  (point-box (div (minpoint box) point)
             (div (maxpoint box) point)))

(defmethod eqv ((a box) (b box))
  (and (= (xmin a) (xmin b))
       (= (xmax a) (xmax b))
       (= (ymin a) (ymin b))
       (= (ymax a) (ymax b))))


(defgeneric displace (box point)
  (:method ((box box) point)
    (point-box (add (minpoint box) point)
               (add (maxpoint box) point))))

(defmethod scale ((box box) (scaler number))
  (point-box (scale (minpoint box) scaler)
             (scale (maxpoint box) scaler)))

(defgeneric bounding-box-delegate (object)
  (:documentation
   "An object that provides the bounding box for some other object."))

(defgeneric bounding-box (object)
  (:method (object)
    (bounding-box (bounding-box-delegate object)))
  (:method ((box box))
    box)
  (:method ((point point))
    (point-box point point))
  (:method ((objects sequence))
    (reduce #'combine (map 'vector #'bounding-box objects))))
  
(defun transpose (box)
  (point-box (minpoint box)
             (point (ymax box) (xmax box))))

(defun containsp (box point)
  (and (<= (xmin box) (x point) (xmax box))
       (<= (ymin box) (y point) (ymax box))))

;;; TODO:
;;;
;;;  overlapsp box box
;;;  containsp box point
;;;
