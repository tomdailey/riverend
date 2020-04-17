(defun sq (x) (* x x))

(defun mag (&rest lst)
  (sqrt (apply #'+ (mapcar #'sq lst))))

(defun vmag (v)
  (sqrt (apply #'+ (mapcar #'sq v))))

(defun unitize (v)
  (let ((m (vmag v)))
    (mapcar #'(lambda (x) (/ x m)) v)))

(defun vdif (v1 v2)
  (mapcar #'- v1 v2))

(defun vdot (v1 v2)
  (apply #'+ (mapcar #'* v1 v2)))

(defun v+ (v1 v2) ; how to give this a rest param?
  (mapcar #'+ v1 v2))

(defun vs* (s v)
  (mapcar #'* v (list-copies s (length v))))

(defun vcross (v1 v2)
  (list
   (- (* (nth 2 v1) (nth 1 v2)) (* (nth 1 v1) (nth 2 v2)))
   (- (* (nth 0 v1) (nth 2 v2)) (* (nth 2 v1) (nth 0 v2)))
   (- (* (nth 0 v1) (nth 1 v2)) (* (nth 1 v1) (nth 0 v2)))))

(defun random-vector (magn)
  (random-elt (list
	       (list magn 0 0)
	       (list 0 magn 0)
	       (list 0 0 magn))))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun list-copies (thing n)
  (if (= n 0)
      nil
      (cons thing (list-copies thing (- n 1)))))

(defun average (&rest lst)
  (/ (apply #'+ lst) (length lst)))

(defun pointwise-op (fn &rest list)
  (make-point
   :x (apply fn (mapcar #'x list))
   :y (apply fn (mapcar #'y list))
   :z (apply fn (mapcar #'z list))))

(defun vop (fn &rest list)
  (make-point
   :x (apply fn (mapcar #'x list))
   :y (apply fn (mapcar #'y list))
   :z (apply fn (mapcar #'z list))))
(defun listify (pt)
  (list (x pt) (y pt) (z pt)))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defun pdistance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))
(defun vdistance (v1 v2)
  (vmag (vdif v1 v2)))

(defun minroot (a b c)
  (if (zerop a)
      (/(- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))

(defun mand (a &rest ls)
  (if ls
      (and a (apply #'mand ls))
      a))

