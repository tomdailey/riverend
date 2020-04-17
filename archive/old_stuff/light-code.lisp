(defstruct color
  red
  green
  blue)

(defparameter *world* nil)
(defvar eye (make-point :x 0 :y 0 :z -700))

(defparameter *red* (make-color :red 255 :blue 0 :green 0))
(defparameter *blue* (make-color :red 0 :blue 255 :green 0))
(defparameter *green* (make-color :red 0 :blue 0 :green 255))
(defparameter *white* (make-color :red 255 :blue 255 :green 255))

(defvar colors nil)
(setq colors (list *red* *blue* *green* *white*))

(defvar light1)
(setq light1 (make-point :x 1 :y 0 :z 0))

;; This writes to the file. It literally just determines the color of the points on the
;; camera-plane
(defun tracer (pathname &optional (res 1)) ;; ideal resolution: 10
  (with-open-file (p pathname :direction :output)
    (format p "P3 ~A ~A 255~%" (* res 128) (* res 72))
    (let ((inc (/ res)))
      (do ((y -36 (+ y inc)))
	  ((< (- 36 y) inc))
	(do ((x -64 (+ x inc)))
	    ((< (- 64 x) inc))
	  (destructuring-bind (red green blue)
	      (color-at x y)
	    (format p "~%~3d ~3d ~3d   " red green blue)))))))

(defun color-at (x y) ;; returns the color at the point on the camera plane.
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
		   (- y (y eye))
		   (- 0 (z eye)))
    (sendray eye xr yr zr)))

(defvar env-light)
(setf env-light 1)

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
	(let ((s-color (surface-base-color s))
	      (brightness (+ env-light
			     (lambert s int (x light1) (y light1) (z light1))
			     (lambert s int (* 0.5 xr) (* 0.5 yr) (* 0.5 zr)))))
	  (mapcar #'(lambda (x) (round (* brightness x)))
		  (list (color-red s-color) (color-green s-color) (color-blue s-color))))
	(list 0 0 0))))

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
(defstruct surface
  base-color)

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :base-color c)))
    (push s *world*)
    s))

(defstruct (prism (:include surface))
  center xl yl zl)
(defun defprism (x y z xl yl zl c)
  (let ((s (make-prism
	    :center (make-point :x  x :y y :z z)
	    :base-color c
	    :xl xl :yl yl :zl zl)))
    (push s *world*)
    s))

(defstruct (plane (:include surface))
  coef)
(defun defplane (x y z color)
  (let ((s (make-plane
	    :base-color color
	    :coef (make-point :x x :y y :z z))))
    (push s *world*)
    s))

(defstruct (particle (:include surface))
  pt)
(defun defparticle (x y z c)
  (push (make-particle :pt (make-point :x x :y y :z z)
		       :base-color c)
	*world*))

;; Computing INTERSECTION
(defun intersect (s pt xr yr zr)
  (funcall (typecase s
	     (sphere #'sphere-intersect)
	     (plane #'plane-intersect)
	     (shmangle #'shmangle-intersect)
	     (particle #'particle-intersect))
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

(defun shmangle-intersect (s pt xr yr zr)
  (let ((p1 (shmangle-pt1 s))
	(p2 (shmangle-pt2 s))
	(p3 (shmangle-pt3 s)))
    (if nil
	nil)))

(defun plane-intersect (s pt xr yr zr)
  (let* ((uhat (make-point :x xr :y yr :z zr))
	 (coef (plane-coef s))
	 (term (apply #'+ (listify (vop #'* uhat coef))))
	 (par nil))
    (unless (= term 0)
      (setf par (- (/ (apply #'+ (listify (vop #'* pt coef))) term)))
      (vop #'+ pt (scale uhat par)))))

;; Computing NORMALS
(defun normal (s pt)
  (funcall (typecase s
	     (sphere #'sphere-normal)
	     (plane #'plane-normal)
	     (particle #'particle-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))
(defun plane-normal (s pt)
  (let ((c (plane-coef s)))
    (unit-vector (x c)
		 (y c)
		 (z c))))
(defun particle-normal (s pt)
  (unit-vector (x eye)
	       (y eye)
	       (z eye)))
