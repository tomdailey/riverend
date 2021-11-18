
(defun half (n)
  (/ n 2))

(defun run-times (n id center-update radius-update)
  (if (< 99999 n)
      (cerror "Continue with n = 99999" "That n is too big.")
      (dotimes (i n)
	(tracer (format nil "~A~3,'0D.png" id i) 10)
	(dolist (ball *world*)
	  (setf (ball-center ball) (funcall center-update (ball-center ball))
		(ball-radius ball) (funcall radius-update (ball-radius ball)))))))

(defun simple-center-move (old-center)
  (mapcar (lambda (x) (+ (half x) (* 0.45 x))) old-center))
(defun simple-radius-shrink (old-radius)
  (* 0.9 old-radius))
