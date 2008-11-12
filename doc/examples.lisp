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
;;; $Id: examples.lisp,v 1.4 2007/10/01 19:57:15 xach Exp $

(defpackage #:vecto-examples
  (:use #:cl #:vecto))

(in-package #:vecto-examples)

(defun radiant-lambda (file)
  (with-canvas (:width 90 :height 90)
    (let ((font (get-font "times.ttf"))
          (step (/ pi 7)))
      (set-font font 40)
      (translate 45 45)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 0 0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (save-png file))))

(defun feedlike-icon (file)
  (with-canvas (:width 100 :height 100)
    (set-rgb-fill 1.0 0.65 0.3)
    (rounded-rectangle 0 0 100 100 10 10)
    (fill-path)
    (set-rgb-fill 1.0 1.0 1.0)
    (centered-circle-path 20 20 10)
    (fill-path)
    (flet ((quarter-circle (x y radius)
             (move-to (+ x radius) y)
             (arc x y radius 0 (/ pi 2))))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-line-width 15)
      (quarter-circle 20 20 30)
      (stroke)
      (quarter-circle 20 20 60)
      (stroke))
    (rounded-rectangle 5 5 90 90 7 7)
    (set-gradient-fill 50 90
                       1.0 1.0 1.0 0.7
                       50 20
                       1.0 1.0 1.0 0.0)
    (set-line-width 2)
    (set-rgba-stroke 1.0 1.0 1.0 0.1)
    (fill-and-stroke)
    (save-png file)))

(defun star-clipping (file)
  (with-canvas (:width 200 :height 200)
    (let ((size 100)
          (angle 0)
          (step (* 2 (/ (* pi 2) 5))))
      (translate size size)
      (move-to 0 size)
      (dotimes (i 5)
        (setf angle (+ angle step))
        (line-to (* (sin angle) size)
                 (* (cos angle) size)))
      (even-odd-clip-path)
      (end-path-no-op)
      (flet ((circle (distance)
               (set-rgba-fill distance 0 0
                              (- 1.0 distance))
               (centered-circle-path 0 0 (* size distance))
               (fill-path)))
        (loop for i downfrom 1.0 by 0.05
              repeat 20 do
              (circle i)))
      (save-png file))))

(defun gradient-example (file)
  (with-canvas (:width 200 :height 50)
    (set-gradient-fill 25 0
                       1 0 0 1
                       175 0
                       1 0 0 0)
    (rectangle 0 0 200 50)
    (fill-path)
    (save-png file)))

(defun gradient-bilinear-example (file)
  (with-canvas (:width 200 :height 50)
    (set-gradient-fill 25 0
                       1 0 0 1
                       175 0
                       1 0 0 0
                       :domain-function 'bilinear-domain)
    (rectangle 0 0 200 50)
    (fill-path)
    (save-png file)))

(defun text-paths (file)
  (with-canvas (:width 400 :height 100)
    (set-font (get-font "/tmp/font.ttf") 70)
    (centered-string-paths 200 15 "Hello, world!")
    (set-line-join :round)
    (set-line-cap :round)
    (set-line-width 3)
    (set-dash-pattern #(0 5) 0)
    (stroke-to-paths)
    (set-gradient-fill 0 0   1 0 0 0.5
                       0 100 1 1 1 1)
    (fill-path)
    (save-png file)))
