;;;; styllexeon.asd

(asdf:defsystem #:styllexeon
  :description "A simple wordprocessor that outputs OOXML (docx)."
  :author "John Q. Splittist <splittist@splittist.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:serapeum
               #:uiop

               #:array-utils

               #:docxplora
               #:flexichain
               #:bordeaux-threads
               )
  :serial t
  :components ((:file "packages")
               (:file "cfst")
               (:file "styllexeon")))

(asdf:defsystem #:styllexeon/test
  :depends-on (#:styllexeon)
  :serial t
  :components ((:file "test")))
