;;;; matrix.lisp

(in-package #:vecto-geometry)

(defclass transform-matrix ()
  ((x-scale
    :initarg :x-scale
    :accessor x-scale)
   (y-skew
    :initarg :y-skew
    :accessor y-skew)
   (x-skew
    :initarg :x-skew
    :accessor x-skew)
   (y-scale
    :initarg :y-scale
    :accessor y-scale)
   (x-offset
    :initarg :x-offset
    :accessor x-offset)
   (y-offset
    :initarg :y-offset
    :accessor y-offset))
  (:default-initargs
   :x-scale 1.0
   :y-skew 0.0
   :x-skew 0.0
   :y-scale 1.0
   :x-offset 0.0
   :y-offset 0.0))

(defmethod print-object ((matrix transform-matrix) stream)
  (print-unreadable-object (matrix stream :type t)
    (format stream "~F ~F ~F ~F ~F ~F"
            (x-scale matrix)
            (y-skew matrix)
            (x-skew matrix)
            (y-scale matrix)
            (x-offset matrix)
            (y-offset matrix))))

(defun transform-matrix (a b c d e f)
  (make-instance 'transform-matrix
                 :x-scale a
                 :y-skew b
                 :x-skew c
                 :y-scale d
                 :x-offset e
                 :y-offset f))

(defvar *identity-matrix* (make-instance 'transform-matrix))

(defmacro transform-matrix-bind (lambda-list matrix  &body body)
  (when (/= (length lambda-list) 6)
    (error "Bad lambda-list for MATRIX-BIND: 6 arguments required"))
  (let ((mat (gensym))
        (slots '(x-scale y-skew x-skew y-scale x-offset y-offset)))
    `(let ((,mat ,matrix))
       (let (,@(loop for slot in slots
                     for var in lambda-list
                     collect (list var `(,slot ,mat))))
         ,@body))))

(defgeneric transform-function (transform-matrix)
  (:method ((matrix transform-matrix))
    (transform-matrix-bind (a b c d e f)
        matrix
      (lambda (point)
        (let ((x (x point))
              (y (y point)))
        (point (+ (* a x) (* c y) e)
               (+ (* b x) (* d y) f)))))))

(defgeneric transform (point matrix)
  (:method (point (matrix transform-matrix))
    (funcall (transform-function matrix) point)))

(defmethod mul ((m1 transform-matrix) (m2 transform-matrix))
  (transform-matrix-bind (a b c d e f)
      m1
    (transform-matrix-bind (a* b* c* d* e* f*)
        m2
      (transform-matrix (+ (* a a*)
                           (* b c*))
                        (+ (* a b*)
                           (* b d*))
                        (+ (* c a*)
                           (* d c*))
                        (+ (* c b*)
                           (* d d*))
                        (+ (* e a*)
                           (* f c*)
                           e*)
                        (+ (* e b*)
                           (* f d*)
                           f*)))))

(defun translation-matrix (tx ty)
  (transform-matrix 1 0 0 1 tx ty))

(defun scaling-matrix (sx sy)
  (transform-matrix sx 0 0 sy 0 0))

(defun rotation-matrix (theta)
  (let ((cos (cos theta))
        (sin (sin theta)))
    (transform-matrix cos sin (- sin) cos 0 0)))

(defun skewing-matrix (alpha beta)
  (transform-matrix 1 (tan alpha) (tan beta) 1 0 0))
