;;; Adapted examples from ZPNG website
;;; (namely here: https://www.xach.com/lisp/zpng/#png)
;;; Works if you're in the ZPNG package (get there via ql:quickload xpng then in-package)

(defun draw-rgb (file)
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width 200
                             :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (loop for a from 38 to 255 by 31
	do (loop for b from 10 to 255 by 10
	     do (loop for g from 38 to 255 by 31
		  do (loop for r from 10 to 255 by 10
			do (write-pixel (list r g b a) png)))))
      (finish-png png))))


(defun draw-blacky (file wid hei)
  "Make a png with given name, wid and hei, that draws a blue-green gradient"
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width wid
                             :height hei)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (loop for x from 0 to (- hei 1) by 1
	   do (loop for y from 0 to (- wid 1) by 1
		 do (write-pixel
		     (list 0
			   (ceiling (* 255 (/ x hei)))
			   (ceiling (* 255 (/ y wid)))
			   255)
		     png)))
      (finish-png png))))
