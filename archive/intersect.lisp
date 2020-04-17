;; Computing INTERSECTION
(defun intersect (s pt direc)
  (funcall (typecase s
	     (sphere #'sphere-intersect)
	     (plane #'plane-intersect)
	     (shmangle #'shmangle-intersect)
	     (particle #'particle-intersect))
	   s pt direc))

(defun sphere-intersect (s pt direc)
  (let* ((c (sphere-center s))
	 (n (minroot (vdot direc direc)
		     (* 2 (vdot (vdif pt c) direc))
		     (- (vdot (vdif pt c) (vdif pt c))
			(sq (sphere-radius s))))))
    (if n
	(v+ pt (vs* n direc)))))

(defun poly-intersect (surf ray)
  (dolist (s (poly-otriangles surf)) ; tigle is an oriented triangle a.k.a otriangle
    (let* ((pln (make-plane (otriangle-points)))
	   (pt (plane-intersect pln ray)))
      (multiple-value-bind (u v) (param-tri pt s)
	  (if (and (> 0 v) (> 0 u) (< 1 (+ v u)))
	      (values s pt))))))

(defun param-tri (pt v1 v2 v3)
  (let* ((vhat (vdif v2 v1))
	 (uhat (vdif v3 v1))
	 (norm (vcross uhat vhat))
	 (trans-pt (vdif pt v1))
	 (vperp (vcross vhat norm))
	 (uperp (vcross uhat norm))
	 (denom (vdot vperp uperp)))
    (values (/ (vdot trans-pt vperp) denom)
	    (/ (vdot trans-pt uperp) denom))))

(defun shmangle-intersect (s pt direc)
  (let ((p1 (shmangle-v1 s))
	(p2 (shmangle-v2 s))
	(p3 (shmangle-v3 s)))
    (let ((int (intersect-plane (specify-plane p1 p2 p3) pt direc)))
      (if int
	  (multiple-value-bind (u v) (param-tri int p1 p2 p3)
	    (if (and (> 0 u) (> 0 v) (< 1 (+ u v)))
		int))))))

(defun intersect-plane )
		
    

(defun particle-intersect (s pt xr yr zr)
  (let ((parm nil))
    (if (apply #'mand (list
		  (if (= xr 0)
		      (= (x (particle-pt s)) (x pt))
		      (setf parm (/ (- (x (particle-pt s)) (x pt)) xr)))
		  (if (= yr 0)
		      (= (y (particle-pt s)) (y pt))
		      (if parm
			  (= parm (/ (- (y (particle-pt s)) (y pt)) yr))
			  (setf parm (/ (- (y (particle-pt s)) (y pt)) yr))))
		  (if (= zr 0)
		      (= (z (particle-pt s)) (z pt))
    		      (if parm
			  (= parm (/ (- (z (particle-pt s)) (z pt)) zr))
			  (setf parm (/ (- (z (particle-pt s)) (z pt)) zr))))))
	(progn
	  (format t "something happened with ~A~%" (z (particle-pt s)))
	  (particle-pt s)))))

(defun plane-intersect (s pt xr yr zr)
  (let* ((uhat (make-point :x xr :y yr :z zr))
	 (coef (plane-coef s))
	 (term (apply #'+ (listify (vop #'* uhat coef))))
	 (par nil))
    (unless (= term 0)
      (setf par (- (/ (apply #'+ (listify (vop #'* pt coef))) term)))
      (vop #'+ pt (scale uhat par)))))
