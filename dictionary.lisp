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
  "A list of lists of lists of characters representing the digits 0-9
  in the specified ASCII art format. The index of each element in this
  list corresponds to the digit that it represents. Each element
  contains a list of rows, each of which contains a list of
  characters (i.e. row-major order).")
