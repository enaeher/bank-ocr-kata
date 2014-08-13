(in-package :ocr)

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun string->list (string)
  (coerce string 'list))

(defun list->string (list)
  (coerce list 'string))

(defun flatten-once (list)
  (apply #'append list))

(defun digit-list->integer (digit-list)
  (apply '+
         (loop
            :with digit-list := (reverse digit-list)
            :for digit :in digit-list
            :for exponent :from 0 :below (length digit-list)
            :collecting (* digit (expt 10 exponent)))))

(defun integer->digit-list (integer)
  (loop
     :for exponent :from (floor (log integer 10)) :downto 0
     :collecting (multiple-value-bind (digit new-integer)
                     (floor integer (expt 10 exponent))
                   (setf integer new-integer)
                   digit)))
