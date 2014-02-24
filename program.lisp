#|
 This file is a part of Trainer
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.colleen.mod.trainer)

(defclass program ()
  ((%dictionary :initarg :dictionary :initform (error "Dictionary required.") :accessor dict)
   (%current-set :accessor current-set)
   (%failed-set :accessor failed-set)
   (%attempts :initarg :attempts :initform 1 :accessor attempts)
   (%current-attempts :accessor current-attempts)
   (%repeats :initarg :repeat-failed :initform 0 :accessor repeats)
   (%current-repeats :accessor current-repeats)

   (%fail-count :accessor fail-count)
   (%success-count :accessor success-count)

   (%start-time :accessor start-time)
   (%stop-time :accessor stop-time))
  (:documentation ""))

(defmethod initialize-instance :after ((program program) &rest rest)
  (declare (ignore rest))
  (start program))

(defmethod print-object ((program program) stream)
  (print-unreadable-object (program stream :type T)
    (format stream "~a" (dict program)))
  program)

(defgeneric submit (program guess))
(defgeneric fail (program))
(defgeneric succeed (program))
(defgeneric next (program))
(defgeneric repeat (program))
(defgeneric start (program))
(defgeneric stop (program))

(defvar *ignore-regex* (cl-ppcre:create-scanner "[.?!,]"))

(defmethod submit ((program program) guess)
  (setf guess (string-trim " " (cl-ppcre:regex-replace-all *ignore-regex* guess "")))
  (if (translation-p (first (current-set program)) guess)
      (progn
        (values (succeed program) T T))
      (progn
        (incf (current-attempts program))
        (if (or (eq (attempts program) T)
                (< (current-attempts program) (attempts program)))
            (values (first (current-set program)) NIL NIL)
            (values (fail program) NIL T)))))

(defmethod fail ((program program))
  (push (first (current-set program)) (failed-set program))
  (incf (fail-count program))
  (next program))

(defmethod succeed ((program program))
  (incf (success-count program))
  (next program))

(defmethod next ((program program))
  (setf (current-attempts program) 0)
  (pop (current-set program))
  (or (first (current-set program))
      (repeat program)))

(defmethod repeat ((program program))
  (incf (current-repeats program))
  (if (or (eq (repeats program) T)
          (<= (current-repeats program) (repeats program)))
      (progn (setf (current-set program) (shuffle (failed-set program))
                   (failed-set program) NIL)
             (or (first (current-set program))
                 (stop program)))
      (stop program)))

(defmethod start ((program program))
  (setf (current-set program) (shuffled-dictionary (dict program))
        (failed-set program) ()
        (current-repeats program) 0
        (current-attempts program) 0
        (fail-count program) 0
        (success-count program) 0
        (start-time program) (get-universal-time))
  (first (current-set program)))

(defmethod stop ((program program))
  (setf (failed-set program) NIL
        (current-set program) NIL
        (current-repeats program) 0
        (current-attempts program) 0
        (stop-time program) (get-universal-time))
  NIL)
