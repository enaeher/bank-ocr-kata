(in-package :ocr)

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun string-to-list (string)
  (coerce string 'list))

(defun list-to-string (list)
  (coerce list 'string))

(defun flatten-once (list)
  (apply #'append list))
