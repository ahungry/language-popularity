(in-package :cl-user)
(defpackage language-popularity-asd
  (:use :cl :asdf))
(in-package :language-popularity-asd)

(defsystem language-popularity
  :version "0.1"
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op language-popularity-test))))
