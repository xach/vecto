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
;;; drawing.lisp

(in-package #:vecto)

(defun gradient-parameter-fun (x0 y0 x1 y1)
  (declare (optimize speed (safety 0)))
  (declare (fixnum x0 y0 x1 y1))
  (lambda (x y)
    (declare (fixnum x y))
    (let ((numerator (+ (* (- x1 x0) (- x x0))
                        (* (- y1 y0) (- y y0))))
          (denominator (+ (expt (- x1 x0) 2)
                          (expt (- y1 y0) 2))))
      (/ numerator  denominator))))

(defun linear-domain (param)
  (clamp-range 0 param 1))

(defun bilinear-domain (param)
  (let ((param (* 2 (clamp-range 0 param 1))))
    (if (<= param 1)
        param
        (- 2 param))))

(defun set-gradient-fill (x0 y0
                          r0 g0 b0 a0
                          x1 y1
                          r1 g1 b1 a1
                          &key
                          (extend-start t)
                          (extend-end t)
                          (domain-function #'linear-domain))
  (declare (optimize speed (safety 0)))
  (declare (inline float-octet))
  (declare (fixnum x0 y0 x1 y1))
  (let* ((matrix (transform-matrix *graphics-state*))
         (fun (make-transform-function (invert-matrix matrix)))
         (gfun (gradient-parameter-fun x0 y0 x1 y1)))
    (setf r0 (float-octet (coerce r0 'single-float))
          g0 (float-octet (coerce g0 'single-float))
          b0 (float-octet (coerce b0 'single-float))
          a0 (float-octet (coerce a0 'single-float))
          r1 (float-octet (coerce r1 'single-float))
          g1 (float-octet (coerce g1 'single-float))
          b1 (float-octet (coerce b1 'single-float))
          a1 (float-octet (coerce a1 'single-float)))
    (setf (fill-source *graphics-state*)
          (lambda (x y)
	    (declare (function fun domain-function))
            (let ((param (multiple-value-call gfun (funcall fun x y))))
	      (declare (fixnum param))
              (cond ((and (< param 0) (not extend-start))
                     (values 0 0 0 0))
                    ((and (< 1 param) (not extend-end))
                     (values 0 0 0 0))
                    (t
                     (setf param (float-octet (coerce (funcall domain-function param) 'single-float)))
                     (values (lerp r0 r1 param)
                             (lerp g0 g1 param)
                             (lerp b0 b1 param)
                             (lerp a0 a1 param)))))))))
