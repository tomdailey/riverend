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
  (push (make-instance 'ball
		       :center cent
		       :radius rad
		       :color col)
	*world*))
(defmethod tran ((a tri) fn)
  (let ((vers (tri-verts a)))
    (make-tri (mapcar fn vers) (surf-color a))))
