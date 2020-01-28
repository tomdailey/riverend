(defparameter *world* nil)
(defvar eye '(0 0 -700))

(defparameter *red* '(255 0 0))
(defparameter *blue* '(0 0 255))
(defparameter *green* '(0 255 0))
(defparameter *white* '(0 0 0))
(defparameter *black* '(255 255 255))
(defvar colors nil)
(setf colors (list *red* *blue* *green* *white*))

(defvar light1)
(setq light1 '(1 0 0))

(defvar env-light)
(setf env-light 0.3)

;; Untested loading of files. Shpest.lisp isn't around yet
;; Put it in the repl with this file, and (make-frame) should work
;; I haven't tested the new file-distribution from a fresh emacs
;; But the points -> 3-lists project is over.
(load "/home/tom/Documents/lisp_programs/riverend/utils.lisp")
(load "/home/tom/Documents/lisp_programs/riverend/normals.lisp")
(load "/home/tom/Documents/lisp_programs/riverend/intersect.lisp")


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
  (let ((ray-direction (unitize (vdif (list x y 0) eye))))
    (sendray eye ray-direction)))

(defun sendray (pt direc)
  (multiple-value-bind (hsurface int-pnt) (first-hit pt direc)
    (if hsurface
	(let ((hsurface-color (surface-base-color hsurface))
	      (brightness (+ env-light
			     (* (- 1 env-light)
				(average (lambert hsurface int-pnt light1)
					 (lambert hsurface int-pnt (vs* 0.15 direc)))))))
	  (mapcar #'(lambda (x) (round (* brightness x))) hsurface-color))
	'(0 0 0))))

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

(defun lambert (s int light) ; Note that light should be a unit vector
  (let ((norm (normal s int)))
    (max 0 (vdot norm light))))

;; Types of shapes
(defstruct surface
  base-color)

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (center r col)
  (let ((s (make-sphere
	    :radius r
	    :center center
	    :base-color col)))
    (push s *world*)
    s))

(defstruct (plane (:include surface))
  coef)
(defun defplane (x y z color)
  (let ((s (make-plane
	    :base-color color
	    :coef (list x y z))))
    (push s *world*)
    s))

(defstruct (particle (:include surface))
  pt)
(defun defparticle (x y z c)
  (push (make-particle :pt (make-point :x x :y y :z z)
		       :base-color c)
	*world*))

(defstruct (shmangle (:include surface))
  v1 v2 v3) 
(defun defshmangle (v1 v2 v3 c) ; give it color and three vertices (as 3-lists)
  (push (make-shmangle :v1 v1
		       :v2 v2
		       :v3 v3
		       :base-color c)
	*world*))
