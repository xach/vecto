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

(defun gradient-parameter (x y x0 y0 x1 y1)
  (let ((numerator (+ (* (- x1 x0) (- x x0))
                      (* (- y1 y0) (- y y0))))
        (denominator (+ (expt (- x1 x0) 2)
                        (expt (- y1 y0) 2))))
    (/ numerator denominator)))

(defun gradient-parameter-fun (x0 y0 x1 y1)
  (lambda (x y)
    (let ((numerator (+ (* (- x1 x0) (- x x0))
                        (* (- y1 y0) (- y y0))))
          (denominator (+ (expt (- x1 x0) 2)
                          (expt (- y1 y0) 2))))
      (/ numerator denominator))))

(defun sawtooth (param)
  (cond ((< param 0)
         (sawtooth (abs param)))
        ((< 1 param)
         (- 1 (nth-value 1 (truncate param))))
        (t param)))

(defun stitch-extend (param)
  (clamp-range 0 param 1))

(defun set-gradient (x0 y0
                     r0 g0 b0 a0
                     x1 y1
                     r1 g1 b1 a1
                     &key (stitch 'stitch-extend))
  (let* ((matrix (transform-matrix *graphics-state*))
         (fun (make-transform-function (invert-matrix matrix)))
         (gfun (gradient-parameter-fun x0 y0 x1 y1)))
    (lambda (x y)
      (let ((param (float-octet (funcall stitch
                                 (multiple-value-call gfun
                                   (funcall fun x y))))))
        (values (lerp r0 r1 param)
                (lerp g0 g1 param)
                (lerp b0 b1 param)
                (lerp a0 a1 param))))))

(defun star (short long points)
  (move-to 0 long)
  (let* ((increment (/ pi points))
         (angle (- (/ pi 4) increment))
         (lengths (vector short long)))
    (dotimes (i (* points 2))
      (let ((length (aref lengths (mod i 2))))
        (line-to (* (cos angle) length)
                 (* (sin angle) length)))
      (incf angle increment))))

(defun test-gradient (file)
  (with-canvas (:width 900 :height 900)
    (translate 400 400)
    (centered-circle-path 0 100 10)
    (stroke)
    (star 50 100 5)
    (set-rgba-fill 0 0 0 0.5)
    (fill-path)
    (save-png file)))
