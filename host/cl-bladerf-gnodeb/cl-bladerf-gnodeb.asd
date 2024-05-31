;;;; cl-bladerf-gnodeb.asd

(asdf:defsystem #:cl-bladerf-gnodeb
  :description "A 5G NR gNode MAC Testbed"
  :author "Tichaona Kadzinga"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-bladerf)
  :components ((:file "package")
               (:file "cl-bladerf-gnodeb")))
