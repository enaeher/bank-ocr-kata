(in-package :ocr)

(defparameter *digits*
  (list
   ;; 0
   '((#\Space #\_ #\Space)
     (#\| #\Space #\|)
     (#\| #\_ #\|))

   ;; 1
   '((#\Space #\Space #\Space)
     (#\Space #\Space #\|)
     (#\Space #\Space #\|))

   ;; 2
   '((#\Space #\_ #\Space)
     (#\Space #\_ #\|)
     (#\| #\_ #\Space))

   ;; 3
   '((#\Space #\_ #\Space)
     (#\Space #\_ #\|)
     (#\Space #\_ #\|))

   ;; 4
   '((#\Space #\Space #\Space)
     (#\| #\_ #\|)
     (#\Space #\Space #\|))

   ;; 5
   '((#\Space #\_ #\Space)
     (#\| #\_ #\Space)
     (#\Space #\_ #\|))

   ;; 6
   '((#\Space #\_ #\Space)
     (#\| #\_ #\Space)
     (#\| #\_ #\|))

   ;; 7
   '((#\Space #\_ #\Space)
     (#\Space #\Space #\|)
     (#\Space #\Space #\|))

   ;; 8
   '((#\Space #\_ #\Space)
     (#\| #\_ #\|)
     (#\| #\_ #\|))

   ;; 9
   '((#\Space #\_ #\Space)
     (#\| #\_ #\|)
     (#\Space #\_ #\|)))
  "A list of two-dimensional arrays of characters representing the
  digits 0-9 in the specified ASCII art format.")

(defparameter *digits-binary*
  (list
   ;; 0
   #2A((nil t nil)
       (t nil t)
       (t t t))

   ;; 1
   #2A((nil nil nil)
       (nil nil t)
       (nil nil t))

   ;; 2
   #2A((nil t nil)
       (nil t t)
       (t t nil))

   ;; 3
   #2A((nil t nil)
       (nil t t)
       (nil t t))

   ;; 4
   #2A((nil nil nil)
       (t t t)
       (nil nil t))

   ;; 5
   #2A((nil t nil)
       (t t nil)
       (nil t t))

   ;; 6
   #2A((nil t nil)
       (t t nil)
       (t t t))

   ;; 7
   #2A((nil t nil)
       (nil nil t)
       (nil nil t))

   ;; 8
   #2A((nil t nil)
       (t t t)
       (t t t))

   ;; 9
   #2A((nil t nil)
       (t t t)
       (nil t t)))
  "A list of two-dimensional arrays of booleans representing the
  digits 0-9 in the specified ASCII art format. (There is not really a
  need to distinguish between the pipe and underscore characters,
  since only pipes will ever be valid in odd-numbered columns, and
  only underscores will ever be valid in even-numbered columns.)")
