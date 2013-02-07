(in-package :cl-user)

(defpackage :system-maid
  (:use :cl :asdf))

(in-package :system-maid)

(defsystem :maid
  :description "Web Server"
  :version "0.0.5"
  :licence "MIT"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (:iolib
               :local-time)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "maid"
                    :depends-on ("package"))))))
