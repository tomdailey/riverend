(defun sq (x) (* x x))

(defun mag (vec)
  (sqrt (apply #'+ (mapcar #'sq vec))))

(defun list-copies (thing n)
  (if (= n 0)
      nil
      (cons thing (list-copies thing (- n 1)))))

(defun vdif (v1 v2)
  (mapcar #'- v1 v2))

(defun vdot (v1 v2)
  (apply #'+ (mapcar #'* v1 v2)))

(defmacro v+ (&rest list-of-args)
  `(mapcar #'+ ,@list-of-args))
(defun vplus (v1 v2)
  (mapcar #'+ v1 v2))

(defun vs* (s v)
  (mapcar #'* v (list-copies s (length v))))

(defun unitize (v)
  (let ((thing (mag v)))
    (if (= thing 0)
	'(1 0 0)
	(vs* (/ 1 (mag v)) v))))

(defun vcross (x y)
  (list
   (- (* (nth 1 x) (nth 2 y)) (* (nth 2 x) (nth 1 y)))
   (- (- (* (nth 0 x) (nth 2 y)) (* (nth 2 x) (nth 0 y))))
   (- (* (nth 0 x) (nth 1 y)) (* (nth 1 x) (nth 0 y)))))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
(defun rand-range (low high)
  (let ((dif (- high low)))
    (+ (random dif) low)))
(defun rand-pt ()
  (list (rand-range -60 60)
	(rand-range -25 25)
	(rand-range -10 10)))
(defun rand-color ()
  (random-elt colors))

(defun average (&rest lst)
  (/ (apply #'+ lst) (length lst)))

(defun vdist (v1 v2)
  (mag (vdif v1 v2)))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))

(defun mand (lst)
  (not (member nil lst)))
 
(defmacro in (obj &rest choices)
  "Tests if obj is in choices. From Graham"
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists"
  (apply #'append (mapcar fn lsts)))

(defmacro while (control &body body)
  `(do ()
       ((not ,control))
     ,@body))
