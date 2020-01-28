(ql:quickload 'zpng)
(use-package 'zpng)

(defun tracer (file res)
  "Make a png with given name, 128x72 at given resolution"
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width (* 128 res)
                             :height (* 72 res)))
	(rescon (/ 1 res)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (do ((x 36 (- x rescon)))
	  ((= x -36))
	(do ((y 64 (- y rescon))) ; we have to write in from the top (high y val's)
	    ((= y -64))
	  (let ((col (color-at x y))) ; col is 4 list
	    (cond
	      ((= 0 (mod y 10))
	       (write-pixel '(100 100 255 255) png))
	      ((= 0 (mod x 10))
	       (write-pixel '(100 100 255 255) png))
	      ((legal-colors-p col)
	       (write-pixel col png))
	      (t (write-pixel '(100 50 0 255) png))))))
      (finish-png png))))


(defun legal-colors-p (color-list)
  (if (rest color-list)
      (and (> 256 (first color-list))
	   (< -1 (first color-list))
	   (legal-colors-p (rest color-list)))
      t))

(defun color-at (x y) ;; returns the color at the point on the camera plane.
  (let ((ray-direction (unitize (vdif (list x y 0) eye))))
    (sendray eye ray-direction)))

(defun sendray (pt direc)
  (multiple-value-bind (hsurface int-pnt) (first-hit pt direc)
    (if hsurface
	(let ((hsurface-col (surface-base-color hsurface))
	      (hsurface-alpha (surface-alpha hsurface))
	      (brightness (lambert hsurface int-pnt direc)))
	  (append (mapcar #'(lambda (x) (round (* brightness x))) hsurface-col)
		  (list hsurface-alpha)))
	'(0 0 0 255)))) ;; if no hsurface hits, make it black and opaque

(defun first-hit (pt direc)
  (let (surface hit dist)
    (dolist (surf *world*)
      (let ((h (intersect surf pt direc)))
	(when h
	  (let ((d (vdistance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface surf
		    hit h
		    dist d))))))
    (values surface hit)))

(defun lambert (s intersec light) ; Note that light should be a unit vector
  (let ((norm (normal s intersec)))
    (max 0 (vdot norm (unitize light)))))

;; Types of shapes
(defstruct surface
base-color
alpha)

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (center r col &optional (alpha 255))
  (let ((s (make-sphere
	    :radius r
	    :center center
	    :base-color col
	    :alpha alpha)))
    (push s *world*)
    s))

(defun intersect (s pt direc)
  (funcall (typecase s
	     (sphere #'sphere-intersect)
	     (plane #'plane-intersect)
	     (shmangle #'shmangle-intersect))
	   s pt direc))

(defun sphere-intersect (s pt direc)
  (let* ((c (sphere-center s))
	 (n (minroot (vdot direc direc)
		     (* 2 (vdot (vdif pt c) direc))
		     (- (vdot (vdif pt c) (vdif pt c))
			(sq (sphere-radius s))))))
    (if n
	(let ((difference (vs* n direc)))
	  (v+ pt difference)))))
		
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

(defun normal (s pt)
  (funcall (typecase s
	     (sphere #'sphere-normal)
	     (plane #'plane-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unitize (vdif c pt))))

