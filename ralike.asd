;;;; Ralike.asd

(asdf:defsystem #:ralike
  :serial t
  :description "Statistische Werkbank angelehnt an R"
  :author "Patrick Krusenotto <patrick.krusenotto@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria #:cl-ppcre)
  :components ((:file "package")
               (:file "ralike")))

