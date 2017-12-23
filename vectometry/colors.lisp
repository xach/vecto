;;;; colors.lisp

(in-package #:vectometry)

(defclass color ()
  ((red
    :initarg :red
    :accessor red)
   (green
    :initarg :green
    :accessor green)
   (blue
    :initarg :blue
    :accessor blue)))

(defun rgb-color (r g b)
  (make-instance 'color :red r :green g :blue b))

(defclass color/alpha (color)
  ((alpha
    :initarg :alpha
    :accessor alpha)))

(defun rgba-color (r g b a)
  (make-instance 'color/alpha :red r :green g :blue b :alpha a))

;;; from kmrcl
(defun rgb->hsv (r g b)
  (let* ((min (min r g b))
         (max (max r g b))
         (delta (- max min))
         (v max)
         (s 0)
         (h nil))
    (when (plusp max)
      (setq s (/ delta max)))
    (when (plusp delta)
      (setq h (cond
               ((= max r)
                (nth-value 0 (/ (- g b) delta)))
               ((= max g)
                (nth-value 0 (+ 2 (/ (- b r) delta))))
               (t
                (nth-value 0 (+ 4 (/ (- r g) delta))))))
      (setq h (* 60 h))
      (when (minusp h)
        (incf h 360)))
    (values h s v)))

(defun hsv->rgb (h s v)
  (when (zerop s)
    (return-from hsv->rgb (values v v v)))

  (loop while (minusp h)
        do (incf h 360))
  (loop while (>= h 360)
        do (decf h 360))

  (let ((h-pos (/ h 60)))
    (multiple-value-bind (h-int h-frac) (truncate h-pos)
      (declare (fixnum h-int))
      (let ((p (* v (- 1 s)))
            (q (* v (- 1 (* s h-frac))))
            (t_ (* v (- 1 (* s (- 1 h-frac)))))
            r g b)

        (cond
         ((zerop h-int)
          (setf r v
                g t_
                b p))
         ((= 1 h-int)
          (setf r q
                g v
                b p))
         ((= 2 h-int)
          (setf r p
                g v
                b t_))
         ((= 3 h-int)
          (setf r p
                g q
                b v))
         ((= 4 h-int)
          (setf r t_
                g p
                b v))
         ((= 5 h-int)
          (setf r v
                g p
                b q)))
        (values r g b)))))

(defun hsv-color (h s v)
  (multiple-value-call 'rgb-color (hsv->rgb h s v))) 

(defgeneric hsv-values (color)
  (:method ((color color))
    (rgb->hsv (red color) (green color) (blue color))))

(defgeneric rgb-values (color)
  (:method ((color color))
    (values (red color) (green color) (blue color))))

(defgeneric darkp (color)
  (:method (color)
    (multiple-value-bind (hue saturation value)
        (hsv-values color)
      (or (< value 0.64)
          (and (< 0.5 saturation)
               (or (< hue 45) (< 205 hue)))))))

(defvar *black* (rgb-color 0 0 0))
(defvar *white* (rgb-color 1 1 1))

(defun contrasting-text-color (color)
  (if (darkp color)
      *white*
      *black*))

(defun add-alpha (color alpha)
  (multiple-value-call #'rgba-color (rgb-values color) alpha))

(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (values (round (* float 255.0))))

(defgeneric html-code (color)
  (:method (color)
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (float-octet (red color))
            (float-octet (green color))
            (float-octet (blue color)))))

(defmethod alpha ((color color))
  1.0)

(defun set-fill-color (color)
  (vecto:set-rgba-fill (red color)
                       (green color)
                       (blue color)
                       (alpha color)))

(defun set-stroke-color (color)
  (vecto:set-rgba-stroke (red color)
                         (green color)
                         (blue color)
                         (alpha color)))

(defun html-color (code)
  (multiple-value-bind (size divisor)
      (ecase (length code)
        (7 (values 2 255.0))
        (4 (values 1 15.0)))
    (flet ((value-at (i)
             (let* ((start (1+ (* i size)))
                    (end (+ start size)))
               (/ (parse-integer code :start start :end end :radix 16)
                  divisor))))
      (rgb-color (value-at 0) (value-at 1) (value-at 2)))))


(defun gray-color (value)
  (rgb-color value value value))

(defun graya-color (value alpha)
  (rgba-color value value value alpha))
