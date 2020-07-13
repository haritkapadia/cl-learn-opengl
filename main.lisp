(load "~/.sbclrc")
(ql:quickload "sdl2")
(ql:quickload "cl-opengl")
(ql:quickload "cl-soil")
(ql:quickload "mathkit")

(load "shader.lisp")
(load "camera.lisp")
(load "vertex-data.lisp")

(setq *random-state* (make-random-state t))

(defmacro random* (lb ub)
  `(+ ,lb (random ,(- ub lb))))

(defmacro set-image (shader-prog texture-id file-path location)
  `(block set-texture-from-image
     (unless (gethash ,location (shader-uniforms ,shader-prog))
       (return-from set-texture-from-image))
     (gl:active-texture ,texture-id)
     (gl:bind-texture :texture-2d ,(intern (concatenate 'string "TEXTURE" (write-to-string texture-id))))
     (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
     (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
     (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
     (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

     (multiple-value-bind (data width height) (cl-soil:load-image ,file-path)
       (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte data)
       (gl:generate-mipmap :texture-2d)
       (cl-soil:free-image-data data))

     (uniformi ,shader-prog ,location ,texture-id)))



;; a cube
(defparameter *vertices* (gl:alloc-gl-array :float (* (+ 3 3 2) 36)))

;; point normal texture
(defparameter fake-vertices (make-array (* (+ 3 3 2) 36) :initial-contents (list
 -0.5 -0.5 -0.5    0.0  0.0 -1.0    0.0 0.0
  0.5 -0.5 -0.5    0.0  0.0 -1.0    1.0 0.0
  0.5  0.5 -0.5    0.0  0.0 -1.0    1.0 1.0
  0.5  0.5 -0.5    0.0  0.0 -1.0    1.0 1.0
 -0.5  0.5 -0.5    0.0  0.0 -1.0    0.0 1.0
 -0.5 -0.5 -0.5    0.0  0.0 -1.0    0.0 0.0
                   
 -0.5 -0.5  0.5    0.0  0.0  1.0    0.0 0.0
  0.5 -0.5  0.5    0.0  0.0  1.0    1.0 0.0
  0.5  0.5  0.5    0.0  0.0  1.0    1.0 1.0
  0.5  0.5  0.5    0.0  0.0  1.0    1.0 1.0
 -0.5  0.5  0.5    0.0  0.0  1.0    0.0 1.0
 -0.5 -0.5  0.5    0.0  0.0  1.0    0.0 0.0
                   
 -0.5  0.5  0.5   -1.0  0.0  0.0    1.0 0.0
 -0.5  0.5 -0.5   -1.0  0.0  0.0    1.0 1.0
 -0.5 -0.5 -0.5   -1.0  0.0  0.0    0.0 1.0
 -0.5 -0.5 -0.5   -1.0  0.0  0.0    0.0 1.0
 -0.5 -0.5  0.5   -1.0  0.0  0.0    0.0 0.0
 -0.5  0.5  0.5   -1.0  0.0  0.0    1.0 0.0
                   
  0.5  0.5  0.5    1.0  0.0  0.0    1.0 0.0
  0.5  0.5 -0.5    1.0  0.0  0.0    1.0 1.0
  0.5 -0.5 -0.5    1.0  0.0  0.0    0.0 1.0
  0.5 -0.5 -0.5    1.0  0.0  0.0    0.0 1.0
  0.5 -0.5  0.5    1.0  0.0  0.0    0.0 0.0
  0.5  0.5  0.5    1.0  0.0  0.0    1.0 0.0
                   
 -0.5 -0.5 -0.5    0.0 -1.0  0.0    0.0 1.0
  0.5 -0.5 -0.5    0.0 -1.0  0.0    1.0 1.0
  0.5 -0.5  0.5    0.0 -1.0  0.0    1.0 0.0
  0.5 -0.5  0.5    0.0 -1.0  0.0    1.0 0.0
 -0.5 -0.5  0.5    0.0 -1.0  0.0    0.0 0.0
 -0.5 -0.5 -0.5    0.0 -1.0  0.0    0.0 1.0
                   
 -0.5  0.5 -0.5    0.0  1.0  0.0    0.0 1.0
  0.5  0.5 -0.5    0.0  1.0  0.0    1.0 1.0
  0.5  0.5  0.5    0.0  1.0  0.0    1.0 0.0
  0.5  0.5  0.5    0.0  1.0  0.0    1.0 0.0
 -0.5  0.5  0.5    0.0  1.0  0.0    0.0 0.0
 -0.5  0.5 -0.5    0.0  1.0  0.0    0.0 1.0)))

(dotimes (i (array-dimension fake-vertices 0))
  (setf (gl:glaref *vertices* i) (aref fake-vertices i)))




(sdl2:with-init (:video)
  (format t "Using SDL Library Version: ~D.~D.~D~%"
          sdl2-ffi:+sdl-major-version+
          sdl2-ffi:+sdl-minor-version+
          sdl2-ffi:+sdl-patchlevel+)
  (finish-output)

  (sdl2:with-window (window :title "The Game" :flags '(:shown :opengl :resizable))
    (sdl2:set-relative-mouse-mode :true)
    ;; needed for modern OpenGL
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask 1)

    (sdl2:with-gl-context (gl-context window)
      ;; double buffering
      (sdl2:gl-set-swap-interval 1)
      (sdl2:gl-make-current window gl-context)
      ;; viewport set
      (multiple-value-bind (width height) (sdl2:get-window-size window)
        (gl:viewport 0 0 width height))
      ;; depth buffer
      (gl:enable :depth-test)

      (let ((cube-data (make-vertex-data* *vertices* (:float . 3) (:float . 3) (:float . 2)))
            (cube-shader (make-program "shaders/cube.vs" "shaders/cube.fs"))
            (light-shader (make-program "shaders/light.vs" "shaders/light.fs"))
            (camera-pos nil)
            (camera-direction nil)
            (camera-right nil)
            (camera-up nil)
            (camera-front nil)
            (width 0)
            (height 0)
            (cube-pos-list (loop for i below 10 collect (sb-cga:vec (float (random 10)) (float (random 10)) (float (random 10)))))
            (point-pos-list (loop for i below 4 collect (sb-cga:vec (+ -1.0 (random 12)) (+ -1.0 (random 12)) (+ -1.0 (random 12)))))
            (spot-pos-list (loop for i below 3 collect (sb-cga:vec (+ 5.0 -3.0 (random 6)) (+ 5.0 -3.0 (random 6)) (+ 5.0 -3.0 (random 6)))))
            (spot-dir-list (loop for i below 3 collect (sb-cga:vec (+ -1.0 (random 2)) (+ -1.0 (random 2)) (+ -1.0 (random 2)))))
            ;; (light-pos (sb-cga:vec 5.0 5.0 5.0))
            (texture0 (gl:gen-texture))
            (texture1 (gl:gen-texture)))

        (when (typep cube-shader 'string)
          (write-string cube-shader)
          (terpri)
          (exit))

        (setf camera-pos #.(sb-cga:vec 0.0 0.0 7.0))
        (setf camera-front #.(sb-cga:normalize (sb-cga:vec -1.0 0.0 -1.0)))
        (setf camera-right (sb-cga:normalize (sb-cga:cross-product #.(sb-cga:vec 0.0 1.0 0.0) camera-front)))
        (setf camera-up (sb-cga:cross-product camera-front camera-right))

        (multiple-value-bind (-width -height) (sdl2:get-window-size window)
          (setf width -width)
          (setf height -height))


        (use-program light-shader)
        (uniform-matrix-4fv light-shader "projection" (kit.math:perspective-matrix (kit.math:deg-to-rad 60.0) (/ width height) 0.1 100.0) nil)
        (uniform-matrix-4fv light-shader "view" (sb-cga:translate* 0.0 1.0 -5.0) nil)
        (uniform-matrix-4fv light-shader "model" (sb-cga:rotate* 0.0 0.0 0.0) nil)
        (uniformf light-shader "lightColor" 1.0 1.0 1.0)

        (use-program cube-shader)
        (set-image cube-shader 0 "assets/container.png" "material.diffuse")
        (set-image cube-shader 1 "assets/container_specular.png" "material.specular")
        (uniform-matrix-4fv cube-shader "projection" (kit.math:perspective-matrix (kit.math:deg-to-rad 60.0) (/ width height) 0.1 100.0) nil)
        (uniform-matrix-4fv cube-shader "view" (sb-cga:translate* 0.0 0.0 -3.0) nil)
        (uniform-matrix-4fv cube-shader "model" (sb-cga:rotate* 0.0 0.0 0.0) nil)
        (uniformf cube-shader "viewPos" (aref camera-pos 0) (aref camera-pos 1) (aref camera-pos 2))

        (let ((i 0))
          (map nil
           (lambda (point-pos)
             (uniformf cube-shader (format nil "points[~d].position" i) (aref point-pos 0) (aref point-pos 1) (aref point-pos 2))
             (uniformf cube-shader (format nil "points[~d].quadratic" i) 0.032)
             (uniformf cube-shader (format nil "points[~d].linear" i) 0.09)
             (uniformf cube-shader (format nil "points[~d].constant" i) 1.0)
             (uniformf cube-shader (format nil "points[~d].light.ambient" i) 0.1 0.1 0.1)
             (uniformf cube-shader (format nil "points[~d].light.diffuse" i) 0.5 0.5 0.5)
             (uniformf cube-shader (format nil "points[~d].light.specular" i) 0.5 0.5 0.5)
             (incf i))
           point-pos-list))
        (let ((i 0))
          (map nil
           (lambda (spot-pos spot-dir)
             (uniformf cube-shader (format nil "spots[~d].position" i) (aref spot-pos 0) (aref spot-pos 1) (aref spot-pos 2))
             (uniformf cube-shader (format nil "spots[~d].direction" i) (aref spot-dir 0) (aref spot-dir 1) (aref spot-dir 2))
             (uniformf cube-shader (format nil "spots[~d].innerCone" i) (cos (kit.math:deg-to-rad (/ 20 2))))
             (uniformf cube-shader (format nil "spots[~d].outerCone" i) (cos (kit.math:deg-to-rad (/ 22 2))))
             (uniformf cube-shader (format nil "spots[~d].quadratic" i) 0.032)
             (uniformf cube-shader (format nil "spots[~d].linear" i) 0.09)
             (uniformf cube-shader (format nil "spots[~d].constant" i) 1.0)
             (uniformf cube-shader (format nil "spots[~d].light.ambient" i) 0.1 0.1 0.1)
             (uniformf cube-shader (format nil "spots[~d].light.diffuse" i) 0.5 0.5 0.5)
             (uniformf cube-shader (format nil "spots[~d].light.specular" i) 0.5 0.5 0.5)
             (incf i))
           spot-pos-list spot-dir-list))
        (uniformf cube-shader "material.ambient" 1.0 1.0 1.0)
        (uniformf cube-shader "material.shininess" 32.0)
        (uniformf cube-shader "directions[0].direction" 1.0 -1.0 1.0)
        (uniformf cube-shader "directions[0].light.ambient" 0.02 0.02 0.02)
        (uniformf cube-shader "directions[0].light.diffuse" 0.08 0.08 0.08)
        (uniformf cube-shader "directions[0].light.specular" 0.1 0.1 0.1)

        (let ((df 0.0)
              (dy 0.0)
              (dh 0.0)
              (yaw 0.0)
              (pitch 0.0)
              (sensitivity 0.15))
          (sdl2:with-event-loop (:method :poll)
            (:windowevent (:event event)
                          (when (= event 5)
                            (multiple-value-bind (-width -height) (sdl2:get-window-size window)
                              (setf width -width)
                              (setf height -height)
                              (gl:viewport 0 0 width height))))

            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym)))
                        (cond
                          ((sdl2:scancode= scancode :scancode-w) (setf df  0.05))
                          ((sdl2:scancode= scancode :scancode-s) (setf df -0.05))
                          ((sdl2:scancode= scancode :scancode-d) (setf dh  0.05))
                          ((sdl2:scancode= scancode :scancode-a) (setf dh -0.05))
                          ((sdl2:scancode= scancode :scancode-e) (setf dy  0.05))
                          ((sdl2:scancode= scancode :scancode-q) (setf dy -0.05)))))

            (:keyup (:keysym keysym)
                    (let ((scancode (sdl2:scancode-value keysym)))
                      (cond
                        ((sdl2:scancode= scancode :scancode-w) (setf df 0.0))
                        ((sdl2:scancode= scancode :scancode-s) (setf df 0.0))
                        ((sdl2:scancode= scancode :scancode-d) (setf dh 0.0))
                        ((sdl2:scancode= scancode :scancode-a) (setf dh 0.0))
                        ((sdl2:scancode= scancode :scancode-e) (setf dy 0.0))
                        ((sdl2:scancode= scancode :scancode-q) (setf dy 0.0)))))

            (:mousemotion (:xrel xrel :yrel yrel)
                          (incf yaw xrel)
                          (incf pitch (* sensitivity (- yrel)))
                          (when (> yaw 360.0)
                            (setf yaw 0.0))
                          (when (< yaw -360.0)
                            (setf yaw -0.0))
                          (when (> pitch 89.0)
                            (setf pitch 89.0))
                          (when (< pitch -89.0)
                            (setf pitch -89.0)))

            (:idle ()
                   (gl:clear-color 0.071 0.129 0.145 1.0)
                   (gl:clear :color-buffer :depth-buffer)

                   (setf camera-front (sb-cga:vec
                                       (* (cos (kit.math:deg-to-rad pitch)) (cos (kit.math:deg-to-rad yaw)))
                                       (* (sin (kit.math:deg-to-rad pitch)))
                                       (* (cos (kit.math:deg-to-rad pitch)) (sin (kit.math:deg-to-rad yaw)))))
                   (setf camera-pos (sb-cga:vec+ (sb-cga:vec* (sb-cga:normalize (sb-cga:vec (aref camera-front 0) 0.0 (aref camera-front 2))) df) camera-pos))
                   (setf camera-pos (sb-cga:vec+ (sb-cga:vec* (sb-cga:normalize (sb-cga:cross-product camera-front camera-up)) dh) camera-pos))
                   (setf camera-pos (sb-cga:vec+ (sb-cga:vec* camera-up dy) camera-pos))

                   (use-program cube-shader)
                   ;; projection matrix can be set outside...
                   (uniform-matrix-4fv cube-shader "projection" (kit.math:perspective-matrix (kit.math:deg-to-rad 60.0) (/ width height) 0.1 100.0) nil)
                   (uniform-matrix-4fv cube-shader "view" (kit.math:look-at camera-pos (sb-cga:vec+ camera-pos camera-front) camera-up) nil)
                   (uniformf cube-shader "viewPos" (aref camera-pos 0) (aref camera-pos 1) (aref camera-pos 2))

                   (map nil
                    (lambda (cube-pos)
                      (uniform-matrix-4fv cube-shader "model" (sb-cga:translate cube-pos) nil)
                      (gl:bind-vertex-array (vertex-data-vao cube-data))
                      (gl:draw-arrays :triangles 0 36))
                    cube-pos-list)

                   (use-program light-shader)
                   (uniform-matrix-4fv light-shader "projection" (kit.math:perspective-matrix (kit.math:deg-to-rad 60.0) (/ width height) 0.1 100.0) nil)
                   (uniform-matrix-4fv light-shader "view" (kit.math:look-at camera-pos (sb-cga:vec+ camera-pos camera-front) camera-up) nil)

                   (map nil
                    (lambda (point-pos)
                      (uniform-matrix-4fv light-shader "model" (sb-cga:matrix* (sb-cga:translate point-pos) (sb-cga:scale* 0.1 0.1 0.1)) nil)
                      (gl:bind-vertex-array (vertex-data-vao cube-data))
                      (gl:draw-arrays :triangles 0 36))
                    point-pos-list)

                   (map nil
                    (lambda (spot-pos spot-dir)
                      (uniform-matrix-4fv light-shader "model"
                                          (sb-cga:matrix*
                                           (sb-cga:translate spot-pos)
                                           (sb-cga:scale* 0.1 0.1 0.1)
                                           (sb-cga:rotate spot-dir))
                                          nil)
                      (gl:bind-vertex-array (vertex-data-vao cube-data))
                      (gl:draw-arrays :triangles 0 36))
                    spot-pos-list spot-dir-list)

                   (sdl2:gl-swap-window window))
            (:quit () t)))

        (gl:delete-vertex-arrays (list (vertex-data-vao cube-data)))
        (gl:delete-buffers (list (vertex-data-vbo cube-data)))
        (format t "cy@~%")))))
