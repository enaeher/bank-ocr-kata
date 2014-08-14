(in-package :ocr)

(defun parse-entry (stream)
  "Reads four lines from STREAM, ignoring the fourth, and returns a
  list of integers representing the parsed digits read from the first
  three lines. If any digit is illegible, nil will appear in the list
  of digits. If it's not possible to read three lines from the stream,
  parse-entry will return nil."
  (let ((strings (loop :for i :from 1 :upto 3 :collecting (read-line stream nil))))
    (prog1
        (when (every 'identity strings)
          (loop
             ;; take sets of three elements
             :for (first second third)
             ;; rotate the lists so that the first three elements are the first three columns
             :on (rotate (mapcar 'string->list strings))
             ;; jump three elements in each iteration
             :by #'cdddr
             ;; rotate them back so that we again have a list of rows to
             ;; compare against the dictionary
             :collecting (position (rotate (list first second third)) *digits* :test #'equal)))
      ;; ignore blank line between entries
      (read-line stream nil))))

(defun parse (stream)
  "Parses all entries in STREAM until EOF is reached, return a list of
  lists of parsed digits."
  (loop
     :for entry := (parse-entry stream)
     :while entry
     :collect entry))

