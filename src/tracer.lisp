(in-package :riverend)

(defparameter *world* nil)
(defvar eye '(0 0 -700))
(defvar *lights1* (list '(0 0 -1)))

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
	(do ((y -64 (+ y rescon))) ; we have to write in from the top (high y val's)
	    ((= y 64))
	  (let ((col (or (board-point y x)
			 (color-at y x))))
	    (cond
	      ((legal-colors-p col)
	       (write-pixel col png))
	      (t (write-pixel '(254 254 255 255) png))))))
      (finish-png png))))

(defun board-point (x y)
  "Scan through *board-functions* to see if x y satisfies any of them"
  (let ((acc nil))
    (dotimes (i (length *board-functions*))
      (push (funcall (nth i *board-functions*) x y) acc))
    (car (member-if-not #'null acc))))

(defun legal-colors-p (color-list)
  "Check if the 4-list has values > 0 and < 256"
  (if (rest color-list)
      (and (> 256 (first color-list))
	   (< -1 (first color-list))
	   (legal-colors-p (rest color-list)))
      t))

(defun color-at (x y)
  "Returns the color at the point on the camera plane."
  (let ((ray-direction (unitize (vdif (cam-plane x y) eye))))
    (sendray eye ray-direction)))
;; TODO add a camera object at this stage
;; it should have: x and y resolution, plane point, eye point

(defvar eye-hyp)
(defvar eye-cos-theta)
(defun update-eye ()
  (setf eye-hyp (reduce #'+ (mapcar #'sq eye))))

(defun cam-plane (x y)
  "Calculate the point on the camera-plane for x y"
  (list x y 0))

(defun sendray (pt direc)
  "Find what color is found by the given ray"
  (multiple-value-bind (hsurfer int-pnt) (first-hit pt direc)
    (if hsurfer
	(let ((hsurfer-col (surf-color hsurfer))
	      (hsurfer-alpha (surf-alpha hsurfer))
	      (brightness (lambert hsurfer int-pnt *lights1*)))
	  (append (mapcar #'(lambda (x) (round (* brightness x))) hsurfer-col)
		  (list hsurfer-alpha)))
	'(0 0 0 255)))) ;; if no hsurfer is hit, make it black and opaque
;; TODO add a bounce function, then sendray on that bounced-ray

(defun first-hit (pt direc)
  "Scan the *world* to find the intersection closest to camera"
  (let (surface hit dist)
    (dolist (surf *world*)
      (let ((h (intersect surf pt direc)))
	(when h
	  (let ((d (vdist h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface surf
		    hit h
		    dist d))))))
    (values surface hit)))

(defun lambert (s intersec lights) 
  "Calculate light factor from given lights"
  (let ((norm (normal s intersec))
	(n (length lights)))
    (/ (reduce #'+ (mapcar (lambda (lux)
			     (let ((ans (vdot norm (unitize lux))))
			       (if (> 0 ans)
				   0
				   (max ans (- ans)))))
			   lights))
       n)))

;; Types of shapes
(defclass surfer ()
  ((color :accessor surf-color
	  :initarg :color
	  :initform *bet-blue*)
   (alpha :accessor surf-alpha
	  :initarg :alpha
	  :initform 255)))

(defclass object ()
  ((trans :accessor obj-trans
	  :initarg :trans
	  :initform (lambda (x)))))
		   
	

