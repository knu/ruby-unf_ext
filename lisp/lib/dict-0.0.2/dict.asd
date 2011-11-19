(in-package :asdf)

(defsystem dict
  :name "dict"
  :version "0.0.2"
  :author "Takeru Ohta"
  :description "An implementation of hash table"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "dict")))