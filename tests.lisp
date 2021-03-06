(in-package :ocr)

;; Utility functions used to generate test data

(defun digit->character-list (digit)
  "Returns a list of characters, given a list of booleans representing
  a digit."
  (mapcar (lambda (row) (mapcar 'boolean->character row '(0 1 2))) digit)) 

(defun digits->ascii (digits)
  "Returns a list of three strings (one per line) representing the
  ASCII art representation of DIGITS, which should be a list of 9
  integers."
  (mapcar 'list->string
          (mapcar 'flatten-once
                  (rotate (loop :for digit :in digits :collecting (digit->character-list (nth digit *digits*)))))))

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
  (loop :for number := (loop :for i :from 0 :below *digits-per-entry* :collecting (random 10))
     :when (checksum-valid-p number)
     :return number))

(defun randomly-corrupt-data (raw-character-list)
  (toggle-bit raw-character-list (random *rows-per-entry*) (random *characters-per-line*)))

;; Test suite proper

(5am:def-suite bank-ocr-kata)
(5am:in-suite bank-ocr-kata)

(5am:test parse-entry
  "Test that we can parse a single randomly-generated entry and get
  the correct value"
  (let ((random-account-number (random-account-number)))
    (with-input-from-string (input (with-output-to-string (output) (write-entry-to-stream random-account-number output)))
      (5am:is (equal random-account-number (parse-entry input))))))

(5am:test parse-500-entries
  "Test that we can parse a stream containing 500 entries (the length
  of an average input file) and get the correct values"
  (let ((random-account-numbers (loop :for i :from 0 :upto 499 :collecting (random-account-number))))
    (with-input-from-string (input (with-output-to-string (output)
                                     (dolist (number random-account-numbers)
                                       (write-entry-to-stream number output))))
      (5am:is (equal random-account-numbers (parse input))))))

(5am:test checksum
  "Test that we correctly calclulate the checksum validity for a
  series of account numbers"
  (let ((valid (mapcar 'integer->digit-list '(888886888
                                              888888880
                                              888888988
                                              555655555
                                              559555555
                                              666566666
                                              686666666
                                              899999999
                                              993999999
                                              999959999
                                              490067115
                                              490067719
                                              490867715
                                              123456789
                                              000000051
                                              490867715)))
        (invalid (mapcar 'integer->digit-list '(888888888
                                                555555555
                                                666666666
                                                999999999
                                                490067715))))
    (5am:is (every 'checksum-valid-p valid))
    (5am:is (notany 'checksum-valid-p invalid))))

(5am:test format
  "Test that we can generate the correct report lines for for a series
  of sample inputs"
  (let ((valid '(8 8 8 8 8 6 8 8 8))
        (invalid '(8 8 8 8 8 8 8 8 8))
        (illegible '(8 8 8 8 nil 8 8 8 8)))
    (5am:is (string= "888886888" (with-output-to-string (s) (write-report-line valid s))))
    (5am:is (string= "888888888 ERR" (with-output-to-string (s) (write-report-line invalid s))))
    (5am:is (string= "8888?8888 ILL" (with-output-to-string (s) (write-report-line illegible s))))))

(5am:test integration
  "Test that we can read in a file full of account numbers and write
  the correct report back out to another file."
  ;; for the sake of simplicity, this test assumes sample-input is in
  ;; the current working directory
  (write-report-to-file "sample-input" "test-output")
  (5am:is (zerop (sb-ext:process-exit-code (sb-ext:run-program "/usr/bin/diff" (list "test-output" "expected-output"))))))

(5am:test toggle-bit
  (let ((sample-digit (copy-tree (nth 2 *digits*))))
    (5am:is (equal '((nil t nil)
                     (nil nil t)
                     (t t nil))
                   (toggle-bit sample-digit 1 1)))
    (5am:is (equal (nth 2 *digits*)
                   sample-digit))))

(5am:test find-valid-alternatives
  "Test that we can correctly find valid alternatives to corrupted data."
  (let ((random-account-numbers (loop :for i :from 0 :upto 20 :collecting (random-account-number))))
    (with-input-from-string (s (with-output-to-string (output)
                                 (dolist (number random-account-numbers)
                                   (write-entry-to-stream number output))))
      (loop :for (digits raw-character-list) := (multiple-value-list (parse-entry s))
         :while (every 'identity raw-character-list)
         :do (5am:is (member digits (find-valid-alternatives (randomly-corrupt-data raw-character-list)) :test 'equal))))))
