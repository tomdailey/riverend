(in-package :riverend)

(defun print-file (file)
  (with-open-file (in file)
    (do ((line nil (read-line in nil 'eof-value)))
	((equal 'eof-value line))
      (princ (string-trim " " line))
      (terpri))))

(defun first-word (string)
  "Return the first space-separated word in string"
  (subseq string 0 (position #\Space string)))

(defmacro pop-word (st)
  "Return the first space-delimited word in string,
  and remove it from the string. DESTRUCTIVE."
  `(let* ((spot (position #\Space ,st)))
     (if spot
	 (let ((word (subseq ,st 0 spot)))
	   (setf ,st (string-trim " " (subseq ,st spot)))
	   word)
	 (if (zerop (length ,st))
	     nil
	     (prog1
		 ,st
	       (setf ,st nil))))))
	       
(defun parse-vertex (string)
  "Parse the vertex line in a standard stl file"
  (let ((stuff (subseq string (position-if #'digit-char-p string)))
	(acc nil))
    (dotimes (i 3)
      (let ((thing (pop-word stuff)))
	(print thing)
	(push (parse-float:parse-float thing) acc)))
    acc))
    
(defun import-stl (filename)
  "Import a standard STL 3d file into a list of triangles"
  (with-open-file (in filename)
    (let ((data nil)
	  (line t))
      (skip-lines in 3)
      (while line
	(let ((acc nil))
	  (dotimes (i 3)
	    (push (parse-vertex (read-line in nil nil)) acc))
	  (push acc data)
	  (setf line (skip-lines in 4))))
      data)))

(defun skip-lines (file n)
  "Skip n lines in file, return nil if eof reached"
  (let ((n (- n 1)))
    (do ((line (read-line file nil nil) (read-line file nil nil))
	 (i 0 (+ i 1)))
	(())
      (if (null line)
	  (return nil)
	  (when (= i n)
	    (return t))))))
  


    
