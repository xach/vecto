;;;; package.lisp

(defpackage #:vecto-geometry
  (:use #:cl)
  (:export #:point
           #:coordinates
           #:apoint
           #:xpoint
           #:ypoint
           #:x #:y
           #:*origin*
           #:midpoint
           #:eqv
           #:add #:sub #:mul #:div #:neg #:abs*
           #:angle
           #:distance
           #:scale
           #:box
           #:bbox-box
           #:point-box
           #:origin-box
           #:bounding-box
           #:bounding-box-delegate
           #:displace
           #:transpose
           #:xmin #:ymin #:xmax #:ymax
           #:centerpoint
           #:maxpoint
           #:minpoint
           #:combine
           #:contract
           #:expand
           #:area
           #:emptyp
           #:containsp
           #:width #:height
           #:*identity-matrix*
           #:transform-matrix
           #:translation-matrix
           #:scaling-matrix
           #:rotation-matrix
           #:skewing-matrix
           #:transform
           #:transform-function))

(defpackage #:vectometry
  (:use #:cl)
  (:use #:vecto-geometry #:vecto)
  (:shadowing-import-from
   #:vecto-geometry
   #:scale)
  (:shadow
   ;; vecto wrappers
   #:move-to
   #:line-to
   #:curve-to
   #:quadratic-to
   #:draw-centered-string
   #:draw-string
   #:string-paths
   #:centered-string-paths
   #:string-bounding-box
   #:arc
   #:arcn
   #:rectangle
   #:rounded-rectangle
   #:centered-ellipse-path
   #:centered-circle-path
   #:set-gradient-fill
   #:translate
   )
  (:export
   ;; colors
   #:rgb-color
   #:rgba-color
   #:hsv-color
   #:red #:green #:blue #:alpha
   #:rgb-values
   #:hsv-values
   #:gray-color
   #:graya-color
   #:*black* #:*white*
   #:html-code
   #:html-color
   #:add-alpha
   #:set-fill-color
   #:set-stroke-color
   ;; extended geometry
   #:top-left
   #:bottom-left
   #:top-right
   #:bottom-right
   #:with-box-canvas
   ;; re-exported geometry bits
   #:point
   #:apoint
   #:xpoint
   #:ypoint
   #:x #:y
   #:*origin*
   #:midpoint
   #:eqv
   #:add #:sub #:mul #:div #:neg #:abs*
   #:angle
   #:distance
   #:box
   #:scale
   #:bbox-box
   #:point-box
   #:origin-box
   #:bounding-box
   #:displace
   #:xmin #:ymin #:xmax #:ymax
   #:centerpoint
   #:maxpoint
   #:minpoint
   #:combine
   #:contract
   #:expand
   #:area
   #:emptyp
   #:containsp
   #:width #:height
   #:*identity-matrix*
   #:transform-matrix
   #:translation-matrix
   #:scaling-matrix
   #:rotation-matrix
   #:skewing-matrix
   #:transform
   #:transform-function
   ;; re-exported vecto bits
   #:with-canvas
   #:clear-canvas
   #:save-png
   #:save-png-stream
   ;; path construction
   #:move-to
   #:line-to
   #:curve-to
   #:quadratic-to
   #:close-subpath
   #:stroke-to-paths
   #:arc
   #:arcn
   ;; Clipping
   #:end-path-no-op
   #:clip-path
   #:even-odd-clip-path
   ;; path construction one-offs
   #:rectangle
   #:rounded-rectangle
   #:centered-ellipse-path
   #:centered-circle-path
   #:+kappa+
   ;; painting
   #:fill-path
   #:even-odd-fill
   #:stroke
   #:fill-and-stroke
   #:even-odd-fill-and-stroke
   ;; graphics state
   #:with-graphics-state
   #:set-line-cap
   #:set-line-join
   #:set-line-width
   #:set-dash-pattern
   #:set-rgba-stroke
   #:set-rgb-stroke
   #:set-rgba-fill
   #:set-rgb-fill
   #:set-gradient-fill
   #:linear-domain
   #:bilinear-domain
   ;; graphics state coordinate transforms
   #:translate
   #:rotate
   #:rotate-degrees
   #:skew
   ;; text
   #:get-font
   #:set-font
   #:draw-string
   #:string-paths
   #:draw-centered-string
   #:centered-string-paths
   #:string-bounding-box))
   
   
(in-package #:vectometry)

