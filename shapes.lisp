;;;; shapes.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:shapes)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

(gl:define-gl-array-format position-normal-color
  (gl:vertex :type :double :components (x y z))
  (gl:normal :type :double :components (nx ny nz))
  (gl:color :type :double :components (r g b a)))

(defparameter *fps* 2)

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

(defun random-triangle ()
  (tri (random-point -120.0 120.0)
       (random-point -120.0 120.0)
       (random-point -120.0 120.0)))


(defun subdivide (tri)
  (with-slots (pt1 pt2 pt3) tri
    (let (
          (mp1 (midpoint pt1 pt2))
          (mp2 (midpoint pt2 pt3))
          (mp3 (midpoint pt1 pt3)))
      (incf (point-z mp1) (randr 3.0 8.25))
      (incf (point-z mp2) (randr 3 8.25))
      (incf (point-z mp3) (randr 3 8.25))
      (list 
       (tri pt1 mp1 mp3)
       (tri mp1 pt2 mp2)
       (tri mp1 mp2 mp3)
       (tri mp2 pt3 mp3)
       ))))

(defun generate-landscape (&optional (iterations 4))
  (let ((results (list (tri (pt -20 -20 0)
                            (pt 80 0 0)
                            (pt 0 80 0)))))
    (dotimes (i iterations)
      (setf results (apply (alexandria:curry #'concatenate 'list) (mapcar #'subdivide results))))

    results))

(defstruct scene
  (triangles (generate-triangles))
  (vertex-array nil)
  (indices-array nil))


(defun generate-buffers-from-triangles (scene &aux (cidx 0))
  (with-slots ((tris triangles) (varray vertex-array) (iarray indices-array)) scene
    (setf varray (gl:alloc-gl-array 'position-normal-color (length tris)))
    (setf iarray (gl:alloc-gl-array :unsigned-int (* (length tris) 3)))
    (labels
        ((fill-pt (pt color normal idx)
           (with-slots (x y z) pt
             (with-slots (red green blue alpha) color
               (with-slots ((nx x) (ny y) (nz z)) normal
                 (setf (gl:glaref iarray idx) idx)
                 (setf (gl:glaref varray idx 'x) x)
                 (setf (gl:glaref varray idx 'y) y)
                 (setf (gl:glaref varray idx 'z) z)
                 (setf (gl:glaref varray idx 'nx) nx)
                 (setf (gl:glaref varray idx 'ny) ny)
                 (setf (gl:glaref varray idx 'nz) nz)
                 (setf (gl:glaref varray idx 'r) red)
                 (setf (gl:glaref varray idx 'g) green)
                 (setf (gl:glaref varray idx 'b) blue)
                 (setf (gl:glaref varray idx 'a) alpha)))))
         (fill-tri (tri idx)
           (with-slots (tcolor pt1 pt2 pt3 normal) tri
             (fill-pt pt1 tcolor normal idx)
             (fill-pt pt1 tcolor normal (+ idx 1))
             (fill-pt pt1 tcolor normal (+ idx 2))
             (+ idx 3))))
      (format t "Generating buffers...~%")
      (dolist (tri tris)
        (setf cidx (fill-tri tri cidx)))
      (format t "Done generating buffers...~%")
      )))

(defun generate-scene ()
  (make-scene :triangles (generate-landscape 0)
              :vertex-array nil
              :indices-array nil))

(defun render-scene (scene)
  (with-slots (vertex-array indices-array) scene
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :color-array)
    (gl:enable-client-state :normal-array)
    (gl:bind-gl-vertex-array vertex-array)
;;    (gl:draw-elements :triangles indices-array)
    (gl:flush)))
    

(define-widget shapes-widget (QGLWidget)
  ((scene :initform (generate-scene)))
  (:documentation "Draw random triangles using OpenGL."))

(define-subwidget (shapes-widget timer) (q+:make-qtimer shapes-widget)
  (setf (q+:single-shot timer) nil))

(define-initializer (shapes-widget setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background shapes-widget) nil)
  (setf (q+:auto-buffer-swap shapes-widget) nil))

(define-slot (shapes-widget tick) ()
  (declare (connected timer (timeout)))
  ;; (setf triangles (generate-triangles))
  (q+:repaint shapes-widget))

(define-slot (shapes-widget regen regenerate) ()
  (declare (connected shapes-widget (regenerate)))
  
  (q+:repaint shapes-widget))

(define-override (shapes-widget initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (shapes-widget resize-g-l) (width height)
  (q+:repaint shapes-widget))

(defun max-radius (scene)
  (* 4.0d0 4.0d0))

(define-override (shapes-widget paint-g-l paint) ()
  "Handle paint events."
  (let* ((max-radius (max-radius scene))
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

      (gl:light :light0 :position (vector 0.0 0.0 10.0 0.0))
      (gl:light :light0 :diffuse (vector 1.0 1.0 1.0 1.0))

      (gl:depth-mask T)
      (gl:depth-func :lequal)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-depth 1.0)
      (gl:front-face :cw)
      (gl:cull-face :back)
      (gl:hint :line-smooth-hint :nicest)

      (gl:enable :line-smooth :polygon-smooth
                 :depth-test :depth-clamp :alpha-test
                 :multisample
                 :lighting :light0
                 )
      (gl:polygon-mode :front-and-back :line)
      (gl:draw-buffer :front-and-back)

      (gl:push-matrix)

      (when (null (scene-vertex-array scene))
        (generate-buffers-from-triangles scene))

      ;; (render-scene scene

      (gl:pop-matrix)

      (q+:swap-buffers shapes-widget)
      (q+:end-native-painting painter))))

(define-override (shapes-widget close-event) (ev)
  (gl:free-gl-array (scene-vertex-array scene))
  (gl:free-gl-array (scene-indices-array scene))
  (q+:accept ev))


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
