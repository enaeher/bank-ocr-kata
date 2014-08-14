(in-package :ocr)

;; Utility functions used to generate test data

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
  (loop :for i :from 0 :to *digits-per-entry* :collecting (random 10)))

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
  (let ((sample-digit
         ;; 2
         '((#\Space #\_ #\Space)
           (#\Space #\_ #\|)
           (#\| #\_ #\Space))))
    (5am:is (equal '((#\Space #\_ #\Space)
                     (#\Space #\Space #\|)
                     (#\| #\_ #\Space))
                   (toggle-bit sample-digit 1 1)))
    (5am:is (equal '((#\Space #\_ #\Space)
                     (#\Space #\_ #\|)
                     (#\| #\_ #\Space))
                   sample-digit))))

(5am:test find-valid-alternatives
  "Test that we can correctly find valid alternatives to corrupted data."
  (let ((corrupted-entry
"    _  _  _  _  _  _     _ 
|_||_|| ||_||_      |  ||_ 
  | _||_||_||_|  |  |  | _|

"))
    (with-input-from-string (s corrupted-entry)
      (multiple-value-bind (digits raw-character-list)
          (parse-entry s)
        (declare (ignore digits))
        (5am:is (member '(4 9 0 8 6 7 7 1 5) (find-valid-alternatives raw-character-list) :test 'equal))))))
