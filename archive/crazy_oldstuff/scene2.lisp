(defun ray-test (filename &optional (res 1))
  (setf *world* nil)
  (defsphere 0 0 700 100 *green*)
  (tracer (make-pathname :name filename) res))

(let ((frame 1))
  (defun reset-frame ()
    (setf frame 0))
  (defun make-frame ()
    (tracer (format nil "blest/blest~4,'0d.png" frame) 5)
    (setf frame (1+ frame))))

(defun run-anim ()
  (let ((start-time (get-universal-time)))
    (do ((i 0 (1+ i)))
	((> i 100))
      (make-frame)
      (format t "Made frame ~d~%" i)
      (update-shit i))
    (let ((elapsed-time (- (get-universal-time) start-time)))
      (format t "Rendered in ~d seconds, or ~2F minutes"
	      elapsed-time
	      (/ elapsed-time 60.0)))))

(defvar light1)
(setf light1 '(1 0 0.5)

(defun update-shit (i)
  ;; update lighting
  (let ((pace 100))
    (setf *light1* (make-point :x (cos (* 2 pi (/ i pace)))
			       :y (sin (* 2 pi (/ i pace)))
			       :z 0.5)))
  ;; update objects
  (let ((nworld ())
	(pace 100))
    (dolist (obj *world*)
      (let ((sphere (make-sphere
		     :radius (sphere-radius obj)
		     :center (make-point
			      :x (* (sin (* 2 pi (/ i pace))) 200)
			      :y 0
			      :z (+ 600 (* (cos (* 2 pi (/ i pace))) 100)))
		     :base-color (sphere-base-color obj))))
	(push sphere nworld)))
    (setf *world* nworld)))

(defun matrix-op (pt op)
  (if (= (array-dimension op 1) (array-dimension pt 0))
      (do ((out nil (cons (do ((j 0 (1+ j))
			       (sum 0 (+ sum (* (aref op i j) (aref pt j)))))
			      ((= j 2 sum)))
			  out))
	   (i 0 (1+ i)))
	  ((= i (array-dimension op 0)) out))
  (format t "Somebody fucked up array dimensions when calling matrix-op")))


(defun new-world ()
  (setq *world* nil)
  (do ((i 0 (1+ i)))
      ((= i 100))
    (defsphere (- (random 800) 400) (- (random 600) 300) (+ (random 500) 700)
	       (+ (random 50) 50)
	       (nth (random 4) colors))))
(let ((count 0))
  (defun next-count () (setf count (1+ count)))
  (defun animate-rand (jump frames)
    (dotimes (i frames)
      (let ((nworld ()))
	(dolist (obj *world*)
	  (let ((sphere (make-sphere
			 :radius (+ (sphere-radius obj) 20)
			 :center (pointwise-op #'+ (make-point
						    :x (+ 0 (random (* 2 jump)))
						    :y (+ 0 (random (* 2 jump)))
						    :z (+ 0 (random (* 2 jump))))
					       (sphere-center obj))
			 :color (sphere-color obj))))
	    (push sphere nworld)))
	(setf *world* nworld))
      (format t "Set ~dth world~%" i)
      (test))))
