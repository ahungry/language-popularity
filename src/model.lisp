(in-package :cl-user)
(defpackage language-popularity.model
  (:use :cl
        :cl-json
        :drakma
        :vecto
        :md5
        :split-sequence)
  (:import-from :language-popularity.config
                :config)
  (:export :get-language-sub-stats
           :pie-chart))

(in-package :language-popularity.model)

(defclass Language ()
  ((Subscribers
    :accessor subs
    :initarg :subs
    :initform 0)
   (Last-Updated
    :accessor last-updated
    :initarg :last-updated
    :initform 0)
   (About
    :accessor about
    :initarg :about
    :initform "Some details about the language."))
  (:documentation "Language stats and details"))

(defparameter *language-stats* (make-hash-table :test #'equal))

(defun char-vector-to-string (v)
  (format nil "~{~a~}" (mapcar #'code-char (coerce v 'list))))

(defun md5-as-string (md5-vector)
  "Convert an md5-vector, as generated by md5sum-file or md5sum-string                                                         into a plain string for easy comparison"
  (string-downcase
   (format nil "~{~2,'0x~}" (coerce md5-vector 'list))))

(defun remote-json-request (uri)
  "Pull in remote JSON.  Drakma returns it as a large vector of
character codes, so we have to parse it out to string form for
cl-json."
  (let* ((json-response-raw (http-request uri))
         (json-response-string (char-vector-to-string json-response-raw))
         (json (decode-json-from-string json-response-string)))
    json))

(defmacro jkey (k &rest rest)
  `(cdr (assoc ,k ,@rest)))

(defconstant +cache-time+ (* 60 60)) ;; 1 hour

(defun set-language-stats (language)
  "Build language stats into our lang class via external sources of
popularity."
  (let ((lang-class (or (gethash language *language-stats*) (make-instance 'Language))))
    (when (> (- (get-universal-time) (last-updated lang-class)) +cache-time+)
      (let ((reddit-json
             (remote-json-request
              (format nil "http://reddit.com/r/~a/about.json"
                      language))))
        (when (jkey :subscribers (jkey :data reddit-json))
          (setf (subs lang-class) (jkey :subscribers (jkey :data reddit-json))))
        (setf (last-updated lang-class) (get-universal-time))))
    (setf (gethash language *language-stats*) lang-class)
    (cons (intern (string-upcase language)) (subs lang-class))))

(defun get-language-sub-stats (language-list)
  "Pull out the stats for a variety of languages listed"
  (sort
   (mapcar #'set-language-stats language-list)
   #'> :key #'cdr))

(defun pie-chart (slices)
  "Parse the data, make a pretty chart"
  (let* ((base (md5-as-string (md5sum-string (format nil "~{~a~}" (mapcar #'car slices)))))
         (file (format nil "~~/src/lisp/language-popularity/static/images/~a.png" base)))
    (with-canvas (:width 400 :height 250)
      (set-rgb-fill 1.0 0 0)
      (centered-circle-path 105 125 100)
      (fill-path)
      (set-font (get-font "~/src/lisp/language-popularity/fonts/kenpixel.ttf") 10)
      (let ((ratios slices)
            (sum (reduce #'+ slices :key #'cdr))
            (last-theta 0))
        (dotimes (i (length ratios))
          (with-graphics-state
            (let* ((ratio (/ (cdr (nth i ratios)) sum))
                   (name (car (nth i ratios)))
                   (t1 last-theta)
                   (t2 (+ t1 (* (/ pi 180) (* 360 ratio)))))
              (setf last-theta t2)
              (flet ((rco () (float (/ (random 100) 100))))
                (set-rgba-fill (rco) (rco) (rco) 1))
              (move-to 105 125)
              (arc 105 125 100 t1 t2)
              (fill-and-stroke)
              (draw-string
               (+ 160 (if (< i 5) (* i 10) (if (> i 10) (- 50 (* (- i 10) 10)) 50)))
               (- 225 (* i 14))
               (format nil "~a  [~a => ~a%]"
                       (prin1-to-string name)
                       (cdr (nth i ratios))
                       (round (* 100 (float ratio)))))
              ))))
      (save-png file)
      base)))
