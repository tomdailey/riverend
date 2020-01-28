;; Computing NORMALS
(defun normal (s pt)
  (funcall (typecase s
	     (sphere #'sphere-normal)
	     (plane #'plane-normal)
	     (particle #'particle-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unitize (vdif pt c))))

(defun plane-normal (s pt)
  (let ((c (plane-coef s)))
    (unit-vector (x c)
		 (y c)
		 (z c))))
(defun particle-normal (s pt)
  (unit-vector (x eye)
	       (y eye)
	       (z eye)))
