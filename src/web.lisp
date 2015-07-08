(in-package :cl-user)
(defpackage language-popularity.web
  (:use :cl
        :caveman2
        :language-popularity.config
        :language-popularity.view
        :language-popularity.db
        :language-popularity.model
        :datafly
        :sxql
        :split-sequence
        :md5)
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

(defroute "/compare/*" (&key splat)
  (let* ((language-list
          (remove "" (split-sequence #\/ (car splat)) :test #'equal))
         (stats (get-language-sub-stats language-list))
         (pie-name (pie-chart stats)))
    (format nil "<img src='/images/~a.png' style='float:left;'>
<div style='font-size:.8em;'>~{~a<br>~%~}</div>"
            pie-name
            (get-language-sub-stats language-list))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
