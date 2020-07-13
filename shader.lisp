(require :cl-opengl)

(defun read-file (name &optional (buffer-size 4096))
  (with-open-file (stream name)
    (let ((buffer (make-array buffer-size :element-type 'character))
          (out ""))
      (do ((read-size (read-sequence buffer stream :start 0 :end buffer-size) (read-sequence buffer stream :start 0 :end buffer-size)))
          ((= read-size 0))
        (setq out (concatenate 'string out buffer)))
      out)))

(defstruct shader program uniforms)

(defun make-program (vertex-shader-file fragment-shader-file)
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (program (gl:create-program))
        (out (make-shader :uniforms (make-hash-table :test 'equal))))
    (gl:shader-source vertex-shader `(,(read-file vertex-shader-file)))
    (gl:compile-shader vertex-shader)
    (let ((shader-log (gl:get-shader-info-log vertex-shader)))
      (unless (string= "" shader-log)
        (return-from make-program shader-log)))

    (setf fragment-shader (gl:create-shader :fragment-shader))
    (gl:shader-source fragment-shader `(,(read-file fragment-shader-file)))
    (gl:compile-shader fragment-shader)
    (let ((shader-log (gl:get-shader-info-log fragment-shader)))
      (unless (string= "" shader-log)
        (return-from make-program shader-log)))

    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)

    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    (setf (shader-program out) program)

    (dotimes (i (gl:get-program program :active-uniforms))
      (let ((uniform-name (multiple-value-bind (len type name) (gl:get-active-uniform program i) name)))
        (setf (gethash uniform-name (shader-uniforms out)) (gl:get-uniform-location program uniform-name))
        (print (cons uniform-name (gethash uniform-name (shader-uniforms out))))))

    out))

(defmacro use-program (program)
  `(gl:use-program (shader-program ,program)))

(defmacro uniform-matrix-4fv (program location matrix &optional (transpose t))
  `(gl:uniform-matrix-4fv (gethash ,location (shader-uniforms ,program)) ,matrix ,transpose))

(defmacro uniformf (program location x &optional y z w)
  `(gl:uniformf (gethash ,location (shader-uniforms ,program)) ,x ,y ,z ,w))

(defmacro uniformi (program location x &optional y z w)
  `(gl:uniformi (gethash ,location (shader-uniforms ,program)) ,x ,y ,z ,w))
