;;;; shapes.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:shapes)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

(defparameter *fps* 2)
(defparameter *fft-window-size* 1024)

(defun draw-point (pt)
  (with-slots (x y z) pt
    (gl:vertex x y z)))

(defun draw-normal (norm)
  (with-slots (x y z) norm
    (gl:normal x y z)))

(defun set-color (color)
  (with-slots (red green blue alpha) color
    (let ((color 
           (make-array 4 :element-type 'double-float :initial-contents (list red green blue alpha))))
      (gl:material :front-and-back :diffuse color)
      (gl:material :front-and-back :ambient #( 0.2 0.2 0.2 0.2))
      )))

(defun draw-tri (triangle)
  (with-slots (pt1 pt2 pt3 tcolor) triangle
    (with-slots ((x1 x) (y1 y) (z1 z)) pt1
      (with-slots ((x2 x) (y2 y) (z2 z)) pt2
        (with-slots ((x3 x) (y3 y) (z3 z)) pt3
          (gl:normal (- (* (- y2 y1) (- z3 z1) (* (- y3 y1) (- z2 z1))))
                     (- (* (- z2 z1) (- x3 x1) (* (- x2 x1) (- z3 z1))))
                     (- (* (- x2 x1) (- y3 y1) (* (- x3 x1) (- y2 y1)))))
          ;;(gl:polygon-mode :front-and-back :fill)
          (set-color tcolor)
          (gl:vertex x1 y1 z1)
          (gl:vertex x2 y2 z2)
          (gl:vertex x3 y3 z3))))))

(defun randr (min max)
  (+ min (random (- max min))))

(defun pt (x y z)
  (geometry:make-point :x (coerce x 'double-float) :y (coerce y 'double-float) :z (coerce z 'double-float)))

(defun random-point (&optional (min-val 4.0) (max-val 5.0))
  (pt (randr min-val max-val)
      (randr min-val max-val)
      (randr min-val max-val)))


(defun tri (pt1 pt2 pt3)
  (geometry:make-triangle
     :normal (tri-normal pt1 pt2 pt3)
     :pt1 pt1
     :pt2 pt2
     :pt3 pt3
     :tcolor (random-color)))

(defun subdivide (tri)
  (with-slots (pt1 pt2 pt3) tri
    (let (
          (mp1 (midpoint pt1 pt2))
          (mp2 (midpoint pt2 pt3))
          (mp3 (midpoint pt1 pt3)))
      (incf (point-z mp1) (randr -0.125 0.125))
      (incf (point-z mp2) (randr -0.125 0.125))
      (incf (point-z mp3) (randr -0.125 0.125))
      (list 
       (tri pt1 mp1 mp3)
       (tri mp1 pt2 mp2)
       (tri mp1 mp2 mp3)
       (tri mp2 pt3 mp3)
       ))))

(defun random-triangle ()
  (tri (random-point 0 8.0)
       (random-point 0 8.0)
       (pt 0 0 0)))

(defun generate-landscape (&optional (iterations 4))
  
  (let ((results (list (tri (pt 0 0 0)
                            (pt 0 60 0)
                            (pt 60 0 0)))))
    (dotimes (i iterations)
      (setf results (apply (alexandria:curry #'concatenate 'list) (mapcar #'subdivide results))))

    results))

(defun generate-triangles ()
  (generate-landscape 4))

(define-widget shapes-widget (QGLWidget)
  ((triangles :initform (generate-triangles)))
  (:documentation "Draw random triangles using OpenGL."))

(define-subwidget (shapes-widget timer) (q+:make-qtimer shapes-widget)
  (setf (q+:single-shot timer) nil))

(define-initializer (shapes-widget setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background shapes-widget) nil)
  (setf (q+:auto-buffer-swap shapes-widget) nil))

(define-slot (shapes-widget tick) ()
  (declare (connected timer (timeout)))
  (setf triangles (generate-triangles))
  (q+:repaint shapes-widget))

(define-slot (shapes-widget regen regenerate) ()
  (declare (connected shapes-widget (regenerate)))
  
  (q+:repaint shapes-widget))

(define-override (shapes-widget initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (shapes-widget resize-g-l) (width height)
  (q+:repaint shapes-widget))

(defun max-radius (triangles)
  (* 4.0d0 4.0d0))

(define-override (shapes-widget paint-g-l paint) ()
  "Handle paint events."
  (let* ((max-radius (max-radius triangles))
         (width (q+:width shapes-widget))
         (height (q+:height shapes-widget))
         (x-aspect-ratio (if (< height width)
                             (/ height width 1.0d0)
                             1.0d0))
         (y-aspect-ratio (if (< height width)
                             1.0d0
                             (/ width height 1.0d0)))
         )

    (with-finalizing 
        ;; Create a painter object to draw on
        ((painter (q+:make-qpainter shapes-widget)))
      (q+:begin-native-painting painter)
      ;; (gl:viewport 0 0 width height)
      ;; (gl:matrix-mode :projection)
      ;; (gl:load-identity)
      ;; (gl:ortho (* -1 x-aspect-ratio max-radius) (* x-aspect-ratio max-radius)
      ;;           (* -1 y-aspect-ratio max-radius) (* y-aspect-ratio max-radius)
      ;;           -1.0 1.0)

      (gl:viewport 0 0 width height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 80 (/ height width) 1.0 5000.0)
      (glu:look-at 80 80 80
                   0 0 0
                   0 0 1 )

      (gl:clear-color 0 0 0 1)

      (gl:shade-model :smooth)
      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (gl:clear :color-buffer :depth-buffer)

      (gl:light :light0 :position (vector 0.0 0.0 -100.0 0.0))
      (gl:light :light0 :diffuse (vector 1.0 1.0 1.0 1.0))


      (gl:enable :line-smooth :polygon-smooth
                 :depth-test :depth-clamp :alpha-test
                 :cull-face
                 :lighting :light0
                 )
      (gl:cull-face :back)
      (gl:polygon-mode :front-and-back :fill)
      (gl:draw-buffer :front-and-back)

      (gl:push-matrix)

      (gl:with-primitives :triangles
        (dolist (tri triangles)
          (draw-tri tri)))

      (gl:pop-matrix)

      (q+:swap-buffers shapes-widget)
      (q+:end-native-painting painter))))


(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (q+:accept ev))

(define-subwidget (main-window viewer) (make-instance 'shapes-widget)
  )

(define-menu (main-window File)
  (:item ("Regenerate" (ctrl r))
         (regenerate main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "A radial visualiztion of FFT data.")))

(define-slot (main-window regen regenerate) ()
  (declare (connected main-window (regenerate)))
  (setf (slot-value viewer 'triangles) (generate-triangles)))


(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Random 3D Shapes")
  (setf (q+:central-widget main-window) viewer))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
