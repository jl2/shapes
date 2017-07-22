;;;; shapes.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:shapes
  :description "Describe shapes here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (
               ;; From QuickLisp
               #:qt
               #:qtools
               #:qtgui
               #:qtcore
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:trivial-main-thread

               ;; My libraries
               #:geometry
               )
  :serial t
  :components ((:file "package")
               (:file "shapes")))

