;;;; fuzzy.asd

(asdf:defsystem #:fae
  :description "Fuzzy Analysis Engine"
  :author "Your Name <your.name@example.com>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:alexandria #:serapeum #:trivia #:fset)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "sets")
                             (:file "db")))
               (:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
