(in-package :cl-user)
(defpackage language-popularity.web
  (:use :cl
        :caveman2
        :language-popularity.config
        :language-popularity.view
        :language-popularity.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :language-popularity.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
