(in-package :riverend)

(defun make-grid ()
  (dotimes (x 10)
    (dotimes (y 10)
      (make-ball (list (+ -64 (* 12.8 x))
		       (+ -36 (* y 7.2))
		       -20)
		 3 (generate-color-from-red)))))

(defun shift-world (direction-values)
  (dolist (obj *world*)
    (setf (slot-value obj 'center) (mapcar #'+ direction-values (slot-value obj 'center)))))

(defun apply-update-to-obj-centers (func)
  (dolist (obj *world*)
    (setf (slot-value obj 'center) (funcall func (slot-value obj 'center)))))
