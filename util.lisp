(in-package :ocr)

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun string->list (string)
  (coerce string 'list))

(defun list->string (list)
  (coerce list 'string))

(defun flatten-once (list)
  (apply #'append list))
