(in-package :cl-user)
(defpackage language-popularity-test-asd
  (:use :cl :asdf))
(in-package :language-popularity-test-asd)

(defsystem language-popularity-test
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:language-popularity
               :prove)
  :components ((:module "t"
                :components
                ((:file "language-popularity"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
