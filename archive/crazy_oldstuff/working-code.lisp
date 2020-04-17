(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/(- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))

(defstruct surface color)

(defparameter *colors* nil)

(setq *colors* '((RED (255 0 0))
	       (GREEN (0 255 0))
	       (BLUE (0 0 255))))

(defparameter *world* nil)

(defconstant eye (make-point :x 0 :y 0 :z -200))


;; This writes to the file. It literally just determines the color of the points on the
;; camera-plane
(defun tracer (pathname &optional (res 1)) ;; ideal resolution: 10
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 128) (* res 72))
    (let ((inc (/ res)))
      (do ((y -36 (+ y inc)))
	  ((< (- 36 y) inc))
	(do ((x -64 (+ x inc)))
	    ((< (- 64 x) inc))
	  (print (color-at x y) p))))))  

(defun color-at (x y) ;; returns the color at the point on the camera plane.
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
		   (- y (y eye))
		   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
	(* (lambert s int xr yr zr) (surface-color s)) ; notice that lambert passes the same unit vector. This is because the light is at the eye
	0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))


;; Types of shapes

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :color c)))
    (push s *world*)
    s))

;; Computing INTERSECTION
(defun intersect (s pt xr yr zr)
  (funcall (typecase
	       s (sphere #'sphere-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))

;; Computing NORMALS
(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))
