;;;; wo-ledger.asd

(asdf:defsystem #:wo-ledger
  :description "Describe wo-ledger here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :pathname "src/"
  :depends-on (#:cl-ledger)
  :class :package-inferred-system)

(asdf:register-system-packages "cl-ledger"  '(:ledger))

