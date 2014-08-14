(defsystem :bank-ocr-kata
  :license "Public Domain"
  :author "Eli Naeher"
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "dictionary" :depends-on ("package"))
               (:file "util")
               (:file "variables")
               (:file "checksum" :depends-on ("util" "variables"))
               (:file "parse" :depends-on ("dictionary" "util"))
               (:file "report" :depends-on ("parse"))
               (:file "file-io" :depends-on ("report"))
               (:file "tests" :depends-on ("parse" "checksum"))))
