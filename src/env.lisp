(in-package :riverend)

(defvar *objects* nil)
(defvar *new-objects* nil)

(defparameter *red* '(255 0 0))
(defparameter *blue* '(0 0 255))
(defparameter *green* '(0 255 0))
(defparameter *white* '(255 255 255))
(defparameter *black* '(0 0 0))
(defparameter  *bet-blue* '(140 170 200))
(defvar colors (list *red* *blue* *green* *white*))
(defvar *color-palette*
  (list '(14 14 68) '(143 32 134) '(253 213 91) '(148 212 237) '(35 166 218)))

(defvar light1 '(0 0 1))
(defvar env-light  0.1)

(defun reset-world () (setf *world* nil))

;; BOARD
(defvar *board-functions* nil)
(defun clear-board ()
  (setf *board-functions* nil))
(defun add-board (fn)
  (push fn *board-functions*))
(defun grid-board (x y)
  (if (or (= 0 y)
	  (= 0 x)
          (= 0 (mod x 10))
	  (= 0 (mod y 10)))
      '(123 188 250 255)))

;; FRAMES
(let ((frame 0)
      (reso 5)
      (mode 'i))
  (defun flip-mode () (setf mode (not mode)))
  (defun set-frame (&optional (num 0))
    (setf frame num))
  (defun next-frame ()
    (setf frame (1+ frame)))
  (defun set-res (num)
    (setf reso num))
  (defun render-frame ()
    (tracer (format nil "blest/frame~4,'0d.png" frame) reso))
  (defun render (start-frame stop-frame increment)
    (let ((start-time (get-universal-time)))
      (do ((frame start-frame))
	  ((> frame stop-frame))
	(tracer (format nil "blest/frame~4,'0d.png" frame) reso)
	(render-frame)
	(format t "Rendered frame ~d~%" frame)
	(update-stuff frame)
	(incf frame increment))
      (let ((elapsed-time (- (get-universal-time) start-time)))
        (format t "Rendered in ~d seconds, or ~2F minutes"
		elapsed-time
		(/ elapsed-time 60.0))))))

(defun add-ring (depth rad col phs)
  (dotimes (i 10)
    (add-ball (list (* rad (cos (+ phs (* pi 2 (/ i 10)))))
		    (* rad (sin (+ phs (* pi 2 (/ i 10)))))
		    depth)
	      3
	      col)))
(defun update-stuff (frame)
  (let ((depth-int 200)
	(ring-num 10)
	(rad 20))
    (reset-world)
    (dotimes (j 20)
      (dotimes (i ring-num)
	(add-ball (list (* rad (cos (+ (/ frame (+ j 10)) (* pi 2 (/ i ring-num)))))
			(* rad (sin (+ (/ frame (+ j 10)) (* pi 2 (/ i ring-num)))))
			(+ -600 (* j depth-int)))
		  3
		  (nth (mod j 5) cool-colors))))))
		

(setf light1 '(0 1 0))
(defun light-update (frame)
  (setf light1 (list (sin (* pi 2 frame (/ 2 360)))
		     0
		     (cos (* pi 2 frame (/ 2 360))))))

(defun step-camera ()
  (setf (nth 0 eye) (+ (* (nth 0 eye) (cos (* pi 2 (/ 2 360))))
		       (* (nth 2 eye) (- (sin (* pi 2 (/ 2 360))))))
	(nth 2 eye) (+ (* (nth 0 eye) (sin (* pi 2 (/ 2 360))))
		       (* (nth 2 eye) (cos (* pi 2 (/ 2 360)))))))
