(in-package :riverend)

(defparameter *info* *standard-output*
  "The stream which world information is printed to")

(defclass ball (surfer object)
  ((radius :accessor ball-radius
	   :initarg :radius
	   :initform 10)
   (center :accessor ball-center
	   :initarg :center
	   :initform (list 0 0 0))))

(defmethod intersect ((s ball) pt direc)
  (let* ((c (ball-center s))
	 (n (minroot (vdot direc direc)
		     (* 2 (vdot (vdif pt c) direc))
		     (- (vdot (vdif pt c) (vdif pt c))
			(sq (ball-radius s))))))
    (if n
	(let ((difference (vs* n direc)))
	  (vplus pt difference)))))

(defmethod normal ((s ball) pt)
  (let ((c (ball-center s)))
    (unitize (vdif pt c))))

(defun add-ball (cent rad col)
  (add-to-world (make-ball cent rad col)))
  
(defun make-ball (cent rad col)
  (push (make-instance 'ball
		       :center cent
		       :radius rad
		       :color col)
	*world*))

(defmethod tprint ((s ball))
  "Print object to your taste"
  (format *info* "Ball center: ~A~%" (ball-center s))
  (format *info* "Ball color: ~A~%" (surf-color s)))
