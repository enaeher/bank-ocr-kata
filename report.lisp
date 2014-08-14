(in-package :ocr)

(defun write-report-line (digits stream)
  "Write a report line for DIGITS, which should be a list of nine
  integers or nils (in the case that a given character is illegible),
  to STREAM."
  (dolist (digit digits)
    (princ (or digit #\?) stream))
  (if (legible-p digits)
      (unless (checksum-valid-p digits)
        (princ " ERR" stream))
      (princ " ILL" stream))
  (values))

(defun write-report (digits-list stream)
  "Write a report for DIGITS-LIST, which should be a list of lists of
  digits, to STREAM."
  (dolist (digits digits-list)
    (write-report-line digits stream)
    (terpri stream)))
