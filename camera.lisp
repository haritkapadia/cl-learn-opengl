(defstruct camera position direction)


(defun look-at (cam)
  (let* ((right (sb-cga:normalize (sb-cga:cross-product (sb-cga:vec 0.0 1.0 0.0) (camera-direction cam))))
         (up (sb-cga:normalize (sb-cga:cross-product (camera-direction cam) right))))
    (let ((rx (aref right 0))
          (ry (aref right 1))
          (rz (aref right 2))
          (ux (aref up 0))
          (uy (aref up 1))
          (uz (aref up 2))
          (dx (aref (camera-direction cam) 0))
          (dy (aref (camera-direction cam) 1))
          (dz (aref (camera-direction cam) 2))
          (npx (- (aref (camera-position cam) 0)))
          (npy (- (aref (camera-position cam) 1)))
          (npz (- (aref (camera-position cam) 2))))
      (sb-cga:matrix rx ry rz 0.0
                     ux uy uz 0.0
                     dx dy dz 0.0
                     npx npy npz 1.0))))

(defmacro translate (cam vec)
  `(setf (camera-position ,cam) (sb-cga:vec+ (camera-position ,cam) ,vec)))

(defmacro set-rotate (cam pitch yaw)
  `(setf (camera-direction ,cam) (sb-cga:vec (* (cos ,pitch) (cos ,yaw)) (* (sin ,pitch)) (* (cos ,pitch) (sin ,yaw)))))
