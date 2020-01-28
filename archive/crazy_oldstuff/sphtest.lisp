(let ((frame 1)
      (directory "thursday")
      (res 5))
  (defun reset-frame (&optional (x 0))
    (setf frame x))
  (defun change-res (n)
    (setf res n))
  (defun make-frame ()
    (tracer (format nil "/home/tom/Documents/lisp_programs/riverend/~A/~A~3,'0d.pgm" directory directory frame) res)
    (setf frame (1+ frame))))

(defun run-anim (&key interval)
  "Runs the animation specified by update-shit, and keeps track of frames/time"
  (let ((start-time (get-universal-time)))
    (do ((i 0 (+ i interval)))
	((> i 10))
      (make-frame)
      (format t "Made frame ~d~%" i)
      (update-shit i))
    (let ((elapsed-time (- (get-universal-time) start-time)))
      (format t "Rendered in ~d seconds, or ~2F minutes"
	      elapsed-time
	      (/ elapsed-time 60.0)))))

(defvar light1)
(setf light1 '(1 0 0.5))

(defun update-shit (i)
  ;; update lighting
  (let ((pace 100))
    (setf *light1* (list (cos (* 2 pi (/ i pace)))
			 (sin (* 2 pi (/ i pace)))
			 0.5)))
  ;; update objects
  (let ((nworld ())
	(pace 100))
    (dolist (obj *world*)
      (let ((sphere (make-sphere
		     :radius (sphere-radius obj)
		     :center (list
			      (* (sin (* 2 pi (/ i pace))) 200)
			      0
			      (+ 600 (* (cos (* 2 pi (/ i pace))) 100)))
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

(defun new-world (n)
  (setq *world* nil)
  (do ((i 0 (1+ i)))
      ((= i n))
    (defsphere
	(list (- (random 800) 400) (- (random 600) 300) (+ (random 500) 700))
	(+ (random 50) 50)
      (nth (random 4) colors)))
  *world*)

(let ((count 0))
  (defun next-count () (setf count (1+ count)))
  (defun animate-rand (jump frames)
    (dotimes (i frames)
      (let ((nworld ()))
	(dolist (obj *world*)
	  (let ((sphere (make-sphere
			 :radius (+ (sphere-radius obj) 20)
			 :center (v+ (sphere-center obj)
				     (random-vector jump))
			 :base-color (sphere-base-color obj))))
	    (push sphere nworld)))
	(setf *world* nworld))
      (format t "Set ~dth world~%" i)
      (make-frame))))
