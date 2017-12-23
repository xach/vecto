;;;; vectometry.asd

(asdf:defsystem #:vectometry
  :author "Zach Beane <xach@xach.com>"
  :description "2d drawing to PNGs with a more object-y interface than Vecto."
  :license "BSD"
  :depends-on (#:vecto)
  :serial t
  :components ((:file "package")
               (:file "point")
               (:file "box")
               (:file "matrix")
               (:file "operations")
               (:file "vectometry")
               (:file "colors")
               (:file "box-text")))

