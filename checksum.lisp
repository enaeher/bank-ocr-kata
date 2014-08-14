(in-package :ocr)

(defun checksum-valid-p (digits)
  "Computes the checksum for DIGITS, which should be a list of nine
  integers."
  (let ((digits (reverse digits)))
    (zerop (mod (apply '+ (loop
                             :for digit :in digits
                             :for i :from 1 :upto *digits-per-entry*
                             :collecting (* i digit)))
                11))))
