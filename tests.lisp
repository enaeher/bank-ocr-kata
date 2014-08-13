(in-package :ocr)

(defun digits->ascii (digits)
  "Returns a list of three strings (one per line) representing the
ASCII art representation of DIGITS, which should be a list of 9
integers."
  (mapcar 'list->string
          (mapcar 'flatten-once
                  (rotate (loop :for digit :in digits :collecting (nth digit *digits*))))))

(defun write-digits-to-stream (digits stream)
  "Writes an ASCII art representation of DIGITS (a list of 9 integers)
to STREAM."
  (dolist (string (digits->ascii digits))
    (princ string stream)
    (terpri stream)))

(defun write-entry-to-stream (digits stream)
  "Writes a full entry for DIGITS (a list of 9 integers) to stream,
including the blank line after the digits."
  (write-digits-to-stream digits stream)
  (terpri stream))

(defun random-account-number ()
  (loop :for i :from 0 :to 9 :collecting (random 10)))

(5am:def-suite bank-ocr-kata)
(5am:in-suite bank-ocr-kata)

(5am:test parse-entry
  (let ((random-account-number (random-account-number)))
    (with-input-from-string (input (with-output-to-string (output) (write-entry-to-stream random-account-number output)))
      (5am:is (equal random-account-number (parse-entry input))))))

(5am:test parse-500-entries
  (let ((random-account-numbers (loop :for i :from 0 :upto 499 :collecting (random-account-number))))
    (with-input-from-string (input (with-output-to-string (output)
                                     (dolist (number random-account-numbers)
                                       (write-entry-to-stream number output))))
      (5am:is (equal random-account-numbers (parse input))))))

