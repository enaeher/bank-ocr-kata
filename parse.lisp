(in-package :ocr)

(defun %parse-character-list (character-list)
  (loop
     ;; take sets of three elements
     :for (first second third)
     ;; rotate the lists so that the first three elements are the first three columns
     :on (rotate character-list)
     ;; jump three elements in each iteration
     :by #'cdddr
     ;; rotate them back so that we again have a list of rows to
     ;; compare against the dictionary
     :collecting (position (rotate (list first second third)) *digits* :test #'equal)))

(defun parse-entry (stream)
  "Reads four lines from STREAM, ignoring the fourth, and returns a
  list of integers representing the parsed digits read from the first
  three lines. If any digit is illegible, nil will appear in the list
  of digits. If it's not possible to read three lines from the stream,
  parse-entry will return nil."
  (let* ((strings (loop :for i :from 1 :upto 3 :collecting (mapcar #'character->boolean (string->list (read-line stream nil)))))
         (parsed-account-number (when (every 'identity strings) (%parse-character-list strings))))
    ;; ignore blank line between entries
    (read-line stream nil)
    (values parsed-account-number strings)))

(defun parse (stream)
  "Parses all entries in STREAM until EOF is reached, return a list of
  lists of parsed digits."
  (loop
     :for entry := (parse-entry stream)
     :while entry
     :collect entry))

