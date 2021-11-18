(in-package :riverend)

(defun generate-color-from-red ()
  (let* ((red (random 256))
	 (green (random (- 256 red)))
	 (blue (- 256 red green)))
    (list red green blue)))
