;;;; operations.lisp

(in-package #:vecto-geometry)

(defgeneric combine (a b)
  (:method (a b)
    (box (min (xmin a) (xmin b))
         (min (ymin a) (ymin b))
         (max (xmax a) (xmax b))
         (max (ymax a) (ymax b))))
  (:method ((a null) b)
    (box (xmin b) (ymin b)
         (xmax b) (ymax b)))
  (:method (a (b null))
    (box (xmin a) (ymin a)
         (xmax a) (ymax a))))
