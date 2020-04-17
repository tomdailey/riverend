(in-package :riverend)

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

;; MAIN TRIANGLE FUNCTIONS

(defmethod tri-verts ((a tri))
  "Return a list of the given tri's verts"
  (list (tri-v1 a) (tri-v2 a) (tri-v3 a)))

(defmethod intersect ((a tri) pt dir)
  "Intersect the given ray with the triangle"
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
  "Calc a unit vector normal to the plane that contains the given vertices."
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
    (if (vzerop z)
	(error "There's a co-linear triangle with vertices ~%~A, ~A, ~A"
	       a b c)
	(let* ((sloop (minv (apply #'mat (mapcan #'list u v z))))
	      (params (m* sloop (apply #'vec3 (vdif pt a)))))
	  (values (vx params) (vy params))))))

(defmethod normal ((toon tri) pt)
  (plane-normal (tri-verts toon)))

;; AUXILLIARY TRIANGLE FUNCTIONS

(defmethod tran ((a tri) fn)
  "make a new tri by applying fn to each vertice of given tri"
  (let ((vers (tri-verts a)))
    (make-tri (mapcar fn vers) (surf-color a))))

(defmethod my-rotate ((a tri) axis amount)
  (make-tri (mapcar (lambda (x) (point-rotate x axis amount))
		    (tri-verts a))
	    (surf-color a)))

(defun point-rotate (point axis amount)
  "Point-rotate: Rotate point about given axis, by amount in radians."
  (let ((prod (m* (mrotation axis amount)
		  (convec4 point))))
    (butlast (get-vlist prod))))

(defun convec4 (point)
  (apply #'vec (append point '(1))))
(defun convec3 (point)
  (apply #'vec point))
(defun get-vlist (vect)
  (list (vx vect) (vy vect)
	(vz vect) (vw vect)))

(defun square-point (x1 x2 point)
  "Flip the point over the line between x1 and x2 for making a square."
  (list x1 x2
	(vdif (vplus x1 x2) point)))

(defun make-tri (vers col)
  "Make a triangle from the given list of vertices and color"
  (make-instance 'tri
		 :v1 (nth 0 vers)
		 :v2 (nth 1 vers)
		 :v3 (nth 2 vers)
		 :color col))

(defun basic-tri ()
  (make-tri (list '(0 0 0) '(10 10 0) '(0 10 0)) '(240 200 100)))
(defun lasic-tri ()
  (make-tri (list '(0 0 0) '(10 10 0) '(10 0 0)) '(240 200 100)))

(defun square ()
  (let* ((t1 (make-tri (list '(0 0 0) '(10 0 0) '(10 10 0)) '(240 200 100)))
	 (t2 (make-tri (square-point (tri-v1 t1) (tri-v3 t1) (tri-v2 t1)) *green*)))
    (radd t1)
    (radd t2)))
(defun cube ()
  (let* ((t1 (make-tri (list '(0 0 0) '(10 0 0) '(10 10 0)) '(240 200 100)))
	 (t2 (make-tri (square-point (tri-v1 t1) (tri-v3 t1) (tri-v2 t1)) *green*))
	 (t3 (my-rotate t1 (vec3 1 0 0) (/ pi 2)))
	 (t4 (my-rotate t2 (vec3 1 0 0) (/ pi 2)))
	 (t5 (my-rotate t3 (vec3 1 0 -10) (/ pi 2)))
	 (t6 (my-rotate t4 (vec3 1 0 -10) (/ pi 2)))
	 (t7 (my-rotate t5 (vec3 1 10 -10) (/ pi 2)))
	 (t8 (my-rotate t6 (vec3 0 10 -10) (/ pi 2)))
	 (t9 (my-rotate t7 (vec3 0 0 -1) (/ pi 2)))
	 (t10 (my-rotate t8 (vec3 0 0 -1) (/ pi 2)))
	 (t11 (my-rotate t9 (vec3 0 0 -1) (/ (- pi) 2)))
	 (t12 (my-rotate t10 (vec3 0 0 -1) (/ (- pi) 2))))
    (radd t1)
    (radd t2)
    (radd t3)
    (radd t4)
    (radd t5)
    (radd t6)
    (radd t7)
    (radd t8)
    (radd t9)
    (radd t10)
    (radd t11)
    (radd t12)))

(defun rotest ()
  (let* ((thing (make-tri (list '(-10 0 0)
				'(0 10 0)
				'(5 -10 0))
			 *red*))
	(other (tran thing (lambda (x) (vplus x '(0 0 20))))))
    (radd thing)
    (radd (my-rotate (my-rotate other (vec3 0 0 1) (/ pi 5))
		     +vx+ (/ pi 6)))))

(defun radd (thing)
  (push thing *world*))

(defun add-tri (vers col)
  "make a tri with the params then add to world"
  (radd (make-tri vers col)))

(defmethod tprint ((a tri))
  "Print given object to my taste"
  (print (tri-verts a))
  (print (surf-color a)))
