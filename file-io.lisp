(in-package :ocr)

(defun parse-file (filename)
  (parse (open filename)))

(defun write-report-to-file (input-file output-file)
  (with-open-file (s output-file :direction :output :if-exists :supersede)
    (write-report (parse-file input-file) s)))
