(in-package :ocr)

(defparameter *digits*
  (list
   ;; 0
   '((nil t nil)   ;;  _ 
     (t nil t)     ;; | |
     (t t t))      ;; |_|

   ;; 1
   '((nil nil nil) ;;
     (nil nil t)   ;;   |
     (nil nil t))  ;;   |

   ;; 2
   '((nil t nil)   ;;  _     
     (nil t t)     ;;  _|
     (t t nil))    ;; |_

   ;; 3
   '((nil t nil)   ;;  _
     (nil t t)     ;;  _|
     (nil t t))    ;;  _|

   ;; 4
   '((nil nil nil) ;;
     (t t t)       ;; |_|
     (nil nil t))  ;;   |

   ;; 5
   '((nil t nil)   ;;  _
     (t t nil)     ;; |_
     (nil t t))    ;;  _|

   ;; 6
   '((nil t nil)   ;;  _
     (t t nil)     ;; |_
     (t t t))      ;; |_|

   ;; 7
   '((nil t nil)   ;;  _
     (nil nil t)   ;;   |
     (nil nil t))  ;;   |

   ;; 8
   '((nil t nil)   ;;  _
     (t t t)       ;; |_|
     (t t t))      ;; |_|

   ;; 9
   '((nil t nil)   ;;  _
     (t t t)       ;; |_|
     (nil t t)))   ;;  _|
  
  "A list of lists of lists of booleans representing the digits 0-9 in
  the specified ASCII art format. Spaces are mapped to nil, and
  underscores and pipes are mapped to t. The index of each element in
  this list corresponds to the digit that it represents. Each element
  contains a list of rows, each of which contains a list of
  characters (i.e. row-major order).")
