(defsystem :bank-ocr-kata
  :license "Public Domain"
  :author "Eli Naeher"
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "dictionary" :depends-on ("package"))
               (:file "variables")
               (:file "util" :depends-on ("variables"))
               (:file "checksum" :depends-on ("util"))
               (:file "parse" :depends-on ("dictionary" "util"))
               (:file "report" :depends-on ("parse"))
               (:file "file-io" :depends-on ("report"))
               (:file "alternatives" :depends-on ("parse"))
               (:file "tests" :depends-on ("parse" "checksum"))))
