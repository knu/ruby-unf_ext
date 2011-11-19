(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.2.2"

  :depends-on (:dict)
  
  :serial t
  :components ((:file "global")
               (:file "byte-order")
               (:file "octet-stream")
               (:file "bintrie-builder")
               (:file "double-array-node-allocator")
               (:file "double-array-buffered-output")
               (:file "double-array-builder")
               (:file "dawg")))
