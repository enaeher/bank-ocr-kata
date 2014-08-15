(in-package :ocr)

(defun rotate (list-of-lists)
  "Rotates LIST-OF-LISTS, returning a list of lists where the first
  list contains the first element of each input list, the second list
  contains the second element, and so on."
  (apply #'mapcar #'list list-of-lists))

(defun boolean->character (boolean column-index)
  (if boolean
      (if (eql (mod column-index 3) 1) #\_ #\|)
      #\Space))

(defun character->boolean (character)
  (cond ((eql character #\Space)
         nil)
        ((member character '(#\_ #\|))
         t)
        (t
         (error "Character must be one of space, underscore, or pipe"))))

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

(defun legible-p (digit-list)
  (and (eql (length digit-list) *digits-per-entry*)
       (every #'identity digit-list)))
