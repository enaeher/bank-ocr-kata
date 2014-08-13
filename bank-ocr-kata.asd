(defsystem :bank-ocr-kata
  :license "Public Domain"
  :author "Eli Naeher"
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "dictionary" :depends-on ("package"))
               (:file "util")
               (:file "parse" :depends-on ("dictionary" "util"))
               (:file "tests" :depends-on ("parse"))))
