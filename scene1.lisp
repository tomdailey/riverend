(load "/home/tom/Documents/lisp_programs/riverend/utilities.lisp")
(load "/home/tom/Documents/lisp_programs/riverend/new.lisp")

(defparameter *world* nil)
(defvar eye '(0 0 -700))

(defparameter *red* '(255 0 0))
(defparameter *blue* '(0 0 255))
(defparameter *green* '(0 255 0))
(defparameter *white* '(0 0 0))
(defparameter *black* '(255 255 255))
(defvar colors nil)
(setf colors (list *red* *blue* *green* *white*))

(defvar light1)
(setq light1 '(0 0 1))

(defvar env-light)
(setf env-light 0.1)

(defvar grid-size)
(defvar grid-spaceing)
(defvar grid-point-radius)
(setq grid-size 20)
(setq grid-spacing 30)
(setq grid-point-radius 10)

(defun make-h-grid (hei size spacing radii)
  (let ((start-spot (- 0 (* spacing (round size 2)))))
    (dotimes (i size)
      (defsphere (list hei (+ start-spot (* i spacing)) 0) radii *green*)
      (format t "~A is where I am~%" i))))

(let ((count 0)
      (reso 5)
      (mode 'i))
  (defun set-count (&optional (num 0))
    (setf count num))
  (defun flip-mode () (setf mode (not mode)))
  (defun set-res (num)
    (setf reso num))
  (defun incr () (setf count (+ 1 count)))
  (defun test ()
    (tracer (format nil "test~A.png" count) reso)
    (when mode (incr))))

(defvar *board-functions* nil)
(defun clear-board () (setf *board-functions* nil))
(defun add-board-function (fn)
  (push fn *board-functions*))

(defun sphere-line (x fn)
  (do ((i 0 (1+ i))
       (acc '(0 0 0)))
      ((> i x))
    (defsphere (setf acc (funcall fn acc)) 10 *blue*)))
	      

(defun reset-world () (setf *world* nil))

(defun my-world ()
  (reset-world)
  (dotimes (i 1)
    (make-h-grid (- (* i 10) 50) 10 10 10)))
