#|
 This file is a part of Trainer
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.colleen.mod.trainer)

(defmacro with-id ((idvar &key (eventvar 'event)) &body body)
  `(let ((,idvar (format NIL "~a/~a" (server ,eventvar) (channel ,eventvar))))
     ,@body))

(defun format-time-since (secs)
  (multiple-value-bind (s m h dd yy) (decode-universal-time secs)
    (setf yy (- yy 1) dd (- dd 1) h (- h 1))
    (format NIL "~:[~D years, ~;~*~]~:[~D days, ~;~*~]~:[~D hours, ~;~*~]~:[~D minutes, ~;~*~]~:[~D seconds~;~*~]" (= yy 0) yy (= dd 0) dd (= h 0) h (= m 0) m (= s 0) s)))

(define-module trainer ()
    ((%trainings :initform (make-hash-table :test 'equalp) :accessor trainings))
  (:documentation "Train vocabulary."))

(define-group trainer :documentation "Manage the trainer.")

(define-handler (privmsg-event event) ()
  (with-id (id)
    (when-let ((program (gethash id (trainings module))))
      (multiple-value-bind (next correct) (submit program (message event))
        (if next
            (respond event "~:[Wrong.~;Correct!~] ~{~a~^, ~}?" correct (terms next))
            (progn
              (remhash id (trainings module))
              (respond event "All done! ~a correct, ~a wrong. Time taken: ~a"
                       (success-count program) (fail-count program)
                       (format-time-since (- (stop-time program) (start-time program))))))))))

(define-command train (dictionary &optional (attempts 1) (repeat-failed 0)) (:documentation "Start training a dictionary on this channel.")
  (with-id (id)
    (if (gethash id (trainings module))
        (respond event "Already training on this channel!")
        (progn
          (unless (integerp attempts) (setf attempts (parse-integer attempts)))
          (unless (integerp repeat-failed) (setf repeat-failed (parse-integer repeat-failed)))
          (let ((instance (make-instance 'program :attempts attempts :repeat-failed repeat-failed :dictionary dictionary)))
            (setf (gethash id (trainings module)) instance)
            (respond event "Ready, set, GO!")
            (respond event "> ~{~a~^, ~}?" (terms (start instance))))))))

(define-command (trainer stop) () (:documentation "Stop training on this channel.")
  (with-id (id)
    (if (gethash id (trainings module))
        (progn (remhash id (trainings module))
               (respond event "Training stopped."))
        (respond event "Training isn't active on this channel."))))

(define-command (trainer dictionaries) () (:documentation "List all available dictionaries.")
  (respond event "Defined dictionaries: ~{~a~^, ~}" (hash-table-keys *dictionaries*)))

(define-command (trainer translations) (dictionary term) (:documentation "List all defined translations for a term.")
  (respond event "Translations for ~a: ~{~a~^, ~}" term (term-translations dictionary term)))

(define-command (trainer siblings) (dictionary term) (:documentation "List all sibling terms for a term.")
  (respond event "Siblings for ~a: ~{~a~^, ~}" term (term-siblings dictionary term)))

(define-command (trainer words) (dictionary) (:documentation "List all words in the dictionary.")
  (respond event "All words in ~a: ~{~a~^, ~}" dictionary (mapcar #'car (terms dictionary))))

(define-command (trainer add-dictionary) (name) (:authorization T :documentation "Define a new dictionary.")
  (if (dictionary name)
      (respond event "A dictionary of this name already exists.")
      (progn (setf (dictionary name) ())
             (respond event "Dictionary created."))))

(define-command (trainer add-word) (dictionary term &rest translations) (:authorization T :documentation "Add a new word to the dictionary.")
  (add-word dictionary (list term) translations)
  (respond event "Word added."))

(define-command (trainer add-terms) (dictionary term &rest more-terms) (:authorization T :documentation "Add more terms to an existing word.")
  (apply #'add-terms dictionary term more-terms)
  (respond event "Terms added."))

(define-command (trainer add-translations) (dictionary term &rest more-translations) (:authorization T :documentation "Add more translations to an existing word.")
  (apply #'add-translations dictionary term more-translations)
  (respond event "Translations added."))

(define-command (trainer remove-dictionary) (name) (:authorization T :documentation "Remove an existing dictionary")
  (remhash name *dictionaries*)
  (respond event "Dictionary removed."))

(define-command (trainer remove-word) (dictionary term) (:authorization T :documentation "Remove a word from the dictionary.")
  (remove-word dictionary term)
  (respond event "Words matching ~a removed." term))

(define-command (trainer remove-terms) (dictionary term &rest more-terms) (:authorization T :documentation "Remove terms from a word.")
  (apply #'remove-terms dictionary term more-terms)
  (respond event "Terms removed."))

(define-command (trainer remove-translations) (dictionary term &rest more-translations) (:authorization T :documentation "Remove translations from a word.")
  (apply #'remove-translations dictionary term more-translations)
  (respond event "Translations removed."))
