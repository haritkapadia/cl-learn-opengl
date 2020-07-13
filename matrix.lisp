;; matrix4fv #(16 element array)
(defmacro identity-matrix (&optional matrix)
  (if matrix
      `(make-array 16 :element-type 'float :initial-contents ,matrix)
      #(1.0 0.0 0.0 0.0
        0.0 1.0 0.0 0.0
        0.0 0.0 1.0 0.0
        0.0 0.0 0.0 1.0)))

(defun frustum-matrix (l r b top n f)
  (let ((t1 (* 2 n))
        (t2 (- r l))
        (t3 (- top b))
        (t4 (- f n)))
    (make-array 16 :initial-contents `(,(/ t1 t2) 0.0 0.0 0.0
                                        0.0 ,(/ t1 t3) 0.0 0.0
                                        ,(/ (+ r l) t2) ,(/ (+ top b) t3) ,(/ (+ f n) -1 t4) -1.0
                                        0.0 0.0 ,(* -1 t1 f (/ 1 t4)) 0.0))))

(defun perspective-matrix (fov-y aspect-ratio znear zfar)
  (let* ((ymax (* znear (tan fov-y)))
         (xmax (* ymax aspect-ratio)))
    (frustum-matrix (- xmax) xmax (- ymax) ymax znear zfar)))

(defmacro mulf (place value)
  `(setf ,place (* ,place ,value)))

(defun normalize (vec3)
  (let ((l (sqrt (+ (expt (aref vec3 0) 2) (expt (aref vec3 1) 2) (expt (aref vec3 2) 2))))
        (out (make-array 3 :initial-contents vec3)))
    (dotimes (i 3 out)
      (mulf (aref out i) l))))

(defmacro scale (vec matrix)
  `(let ((out (make-array 16 :initial-contents ,matrix)))
     (mulf (aref out 0)  (aref ,vec 0))
     (mulf (aref out 5)  (aref ,vec 1))
     (mulf (aref out 10) (aref ,vec 2))
     out))

(defmacro translate (vec matrix)
  `(let ((out (make-array 16 :initial-contents ,matrix)))
     (incf (aref out 3)  (aref ,vec 0))
     (incf (aref out 7)  (aref ,vec 1))
     (incf (aref out 11) (aref ,vec 2))
     out))

(defmacro rotate (axis angle matrix)
  (let ((rx `(aref ,axis 0))
        (ry `(aref ,axis 1))
        (rz `(aref ,axis 2))
        (st `(sin ,angle))
        (ct `(cos ,angle))
        (act `(- 1 (cos ,angle))))
    `(let ((out (make-array 16 :initial-contents ,matrix)))
       (incf (aref out 0)  (+ ,ct (* ,rx ,rx ,act))) ;
       (incf (aref out 1)  (- (* ,rx ,ry ,act) (* ,rz ,st))) ;
       (incf (aref out 2)  (+ (* ,rx ,rz ,act) (* ,ry ,st))) ;
       (incf (aref out 4)  (+ (+ ,rx ,ry ,act) (* ,rz ,st))) ;
       (incf (aref out 5)  (+ ,ct (* ,ry ,ry ,act))) ;
       (incf (aref out 6)  (+ (* ,ry ,rz ,act) (* ,rx ,st))) ;
       (incf (aref out 8)  (+ (* ,rx ,rz ,act) (* ,ry ,st))) ;
       (incf (aref out 8)  (+ (* ,ry ,rz ,act) (* ,rx ,st))) ;
       (incf (aref out 10) (+ ,ct (* ,rz ,rz ,act)))
       out)))
