(in-package :ocr)

(defun parse-entry (stream)
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
  (loop
     :for entry := (parse-entry stream)
     :while entry
     :collect entry))

(defun parse-file (filename)
  (parse (open filename)))
