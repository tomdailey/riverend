(defun sphere-line (x fn)
  (do ((i 0 (1+ i))
       (acc '(0 0 0)))
      ((> i x))
    (add-ball (setf acc (funcall fn acc)) 10 *blue*)))

(defun add-tris ()
  "Make three random tris"
  (dotimes (i 3)
    (push (make-instance 'tri
			 :v1 (rand-pt)
			 :v2 (rand-pt)
			 :v3 (rand-pt)
			 :color (rand-color)
			 :trans #'trans-tri-10)
	  *world*)))

(defun add-balls ()
  (dotimes (i 3)
    (push (make-instance 'ball
			 :center (rand-pt)
			 :radius (rand-range 3 20)
			 :color (rand-color))
	  *world*)))

(defun red-board (x y)
  '(255 100 100 255))

(defun my-world ()
  (reset-world)
  (dotimes (i 1)
    (make-h-grid (- (* i 10) 50) 10 10 10)))
(defun my-better-world ()
  (reset-world)
  (add-balls)
  (add-tris))
(defun make-sur ()
  (dotimes (i 9)
    (dotimes (j 9)
      (push
       (make-tri (list (list i (aref mr i j) j)
		       (list i (aref mr (1+ i) j) j)
		       (list i (aref mr i j) j)) *red*)
       *intlist*)
      (push
       (make-tri (list (list i (aref mr i j) j)
		       (list i (aref mr (1+ i) j) j)
		       (list i (aref mr i j) j)) *red*)
       *intlist*))))
