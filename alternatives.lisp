(in-package :ocr)

(defun toggle-bit (character-list row column)
  (let ((copy (copy-tree character-list)))
    (setf (nth column (nth row copy))
          (not (nth column (nth row copy))))
    copy))

(defun find-valid-alternatives (raw-character-list)
  "Returns the list of possible account numbers matching the
  characters in RAW-CHARACTER-LIST, which should be a list of three
  rows, each of which should be a list of characters, if one and only
  one edit were made to that set of characters. Only returns those
  potential matches with a valid checksum."
  (let (alternatives)
    (dotimes (row *rows-per-entry*)
      (dotimes (column *characters-per-line*)
        (let ((potential-account-number (%parse-character-list (toggle-bit raw-character-list row column))))
          (when (and (legible-p potential-account-number)
                     (checksum-valid-p potential-account-number))
            (push potential-account-number alternatives)))))
    alternatives))
