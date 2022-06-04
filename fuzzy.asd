;;;; fuzzy.asd

(asdf:defsystem #:fuzzy
  :description "Describe fuzzy here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:alexandria #:serapeum #:trivia #:fset)
  :components ((:file "package")
               (:file "sets")
               (:file "tests")))
