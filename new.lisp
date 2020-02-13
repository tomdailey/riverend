(defun tracer (file res)
  "Make a png with given name, 128x72 at given resolution"
  (let ((png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width (* 128 res)
                             :height (* 72 res)))
	(rescon (/ 1 res)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (do ((x 36 (- x rescon)))
	  ((= x -36))
	(do ((y -64 (+ y rescon))) ; we have to write in from the top (high y val's)
	    ((= y 64))
	  (let ((col (or (board-point y x)
			 (color-at y x))))
	    (cond
	      ((legal-colors-p col)
	       (zpng:write-pixel col png))
	      (t (zpng:write-pixel '(254 254 255 255) png))))))
      (zpng:finish-png png))))

(defun board-point (x y)
  (let ((acc nil))
    (dotimes (i (length *board-functions*))
      (push (funcall (nth i *board-functions*) x y) acc))
    (car (member-if-not #'null acc))))
   ;; (car (member t acc))))

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
  (multiple-value-bind (hsurfer int-pnt) (first-hit pt direc)
    (if hsurfer
	(let ((hsurfer-col (surf-color hsurfer))
	      (hsurfer-alpha (surf-alpha hsurfer))
	      (brightness (lambert hsurfer int-pnt direc)))
	  (append (mapcar #'(lambda (x) (round (* brightness x))) hsurfer-col)
		  (list hsurfer-alpha)))
	'(0 0 0 255)))) ;; if no hsurfer is hit, make it black and opaque

(defun first-hit (pt direc)
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

(defun lambert (s intersec light) ; Note that light should be a unit vector
  (let ((norm (normal s intersec)))
    (let ((ans (vdot norm (unitize '(1 0 0)))))
      (max ans (- ans)))))

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

(load "/home/tom/Documents/lisp_programs/riverend/ball.lisp")
(load "/home/tom/Documents/lisp_programs/riverend/tri.lisp")
		   
	

