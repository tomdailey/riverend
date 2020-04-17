(in-package :riverend)

(defun make-coords ()
  (add-ball '(0 0 0) 15 *white*)
  (add-ball '(0 20 0) 3 *red*)
  (add-ball '(20 0 0) 3 *blue*))
(defun print-world ()
  (dolist (thing *world*)
    (tprint thing)))
