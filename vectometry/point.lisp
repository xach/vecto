;;;; point.lisp

(in-package #:vecto-geometry)

(defclass point ()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (format stream "~A,~A" (x point) (y point))))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defun coordinates (point)
  (values (x point) (y point)))

(defun xpoint (x)
  (point x 0))

(defun ypoint (y)
  (point 0 y))

(defun apoint (angle distance)
  (point (* distance (cos angle))
         (* distance (sin angle))))

(defgeneric midpoint (a b)
  (:method (a b)
    (point (/ (+ (x a) (x b)) 2)
           (/ (+ (y a) (y b)) 2))))

(defgeneric eqv (a b)
  (:method (a b)
    (and (= (x a) (x b))
         (= (y a) (y b)))))

(defgeneric add (a b &rest args)
  (:method (a b &rest args)
    (if args
        (reduce #'add/2 args :initial-value (add/2 a b))
        (add/2 a b))))

(macrolet ((define-point-op (name operation)
             `(defgeneric ,name (a b)
               (:method ((a point) (b point))
                 (point (,operation (x a) (x b))
                        (,operation (y a) (y b)))))))
  (define-point-op add/2 +)
  (define-point-op sub -)
  (define-point-op mul *)
  (define-point-op div /))

(defgeneric neg (object)
  (:method ((point point))
    (point (- (x point))
           (- (y point)))))

(defgeneric distance (p1 p2)
  (:method ((p1 point) (p2 point))
    (let ((diff (sub p1 p2)))
      (sqrt (+ (* (x diff) (x diff))
               (* (y diff) (y diff)))))))

(defgeneric abs* (object)
  (:method ((point point))
    (point (abs (x point))
           (abs (y point)))))

(defgeneric angle (p1 p2)
  (:method ((p1 point) (p2 point))
    (let* ((diff (sub p2 p1))
           (x (x diff))
           (y (y diff)))
      (if (zerop x)
          (if (plusp y)
              (/ pi 2)
              (* 3 (/ pi 2)))
          (atan y x)))))

(defgeneric scale (object scalar)
  (:method ((point point) scalar)
    (point (* (x point) scalar)
           (* (y point) scalar))))
    

(defvar *origin* (point 0 0))
