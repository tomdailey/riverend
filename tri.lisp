(use-package '3d-vectors)
(use-package '3d-matrices)

(defclass tri (surfer object)
  ((v1 :accessor tri-v1
       :initarg :v1
       :initform '(1 1 0))
   (v2 :accessor tri-v2
       :initarg :v2
       :initform '(2 1 0))
   (v3 :accessor tri-v3
       :initarg :v3
       :initform '(1 2 0))))
(defmethod tran ((a tri) fn)
  (let ((vers (tri-verts a)))
    (make-tri (mapcar fn vers) (surf-color a))))
(defun make-tri (vers col)
  (make-instance 'tri
		 :v1 (nth 0 vers)
		 :v2 (nth 1 vers)
		 :v3 (nth 2 vers)
		 :color col))
(defun add-tri (v1 v2 v3)
  (push (make-instance 'tri
			 :v1 v1
			 :v2 v2
			 :v3 v3
			 :color '(255 230 0))
	  *world*))
(defun basic-tri ()
  (make-tri (list '(0 0 0) '(10 10 0) '(0 10 0)) '(240 200 100)))
(defun lasic-tri ()
  (make-tri (list '(0 0 0) '(10 10 0) '(10 0 0)) '(240 200 100)))

(defmethod tprint ((a tri))
  (print (tri-verts a))
  (print (surf-color a)))
(defmethod tri-verts ((a tri))
  (list (tri-v1 a) (tri-v2 a) (tri-v3 a)))
(defmethod intersect ((a tri) pt dir)/
  (let ((amount (to-plane pt dir (tri-verts a))))
     (if amount
	(let ((testp (vplus pt (vs* amount dir)))) 
	  (multiple-value-bind (x y)
	      (transform-to-tri-coords testp (tri-v1 a) (tri-v2 a) (tri-v3 a))
	    (if (and (and (<= 0 x) (<= 0 y))
		     (and (>= 1 (+ x y))))
		pt
		nil)))
	nil)))
(defun to-plane (pt dir ls)
  "Calculate intersection multiplier in the parametrization of the ray"
  (let* ((n (plane-normal ls))
	 (denom (vdot dir n)))
    (if (= denom 0) ; ray and plane-normal can't be perpendicular, points can't be in a line
       nil ; signal that they don't intersect anywhere
	   ; (or they do everywhere mwah ha ha)
	(/ (vdot (vdif '(0 0 0) (vdif pt (first ls))) n)
	   denom))))
(defun plane-normal (verts)
  "Calc a unit vector normal of the plane that contains the given vertices."
  ;; As of now, you could give a list of n verts, but it only uses the first 3
  ;; They better not be in a line
  (let ((vhat (vdif (second verts) (first verts)))
	(uhat (vdif (third verts) (first verts))))
    (unitize (vcross vhat uhat))))
(defun transform-to-tri-coords (pt a b c)
  "Transform the given point to new basis"
  (let* ((u (vdif b a))
 	 (v (vdif c a))
	 (z (vcross u v)))
    (let ((sloop (minv (apply #'mat (mapcan #'list u v z)))))
      (let ((params (m* sloop (apply #'vec3 (vdif pt a)))))
	(values (vx params) (vy params))))))

(defmethod normal ((toon tri) pt)
  (vdif '(0 0 0) (plane-normal (tri-verts toon))))
