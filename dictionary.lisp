#|
 This file is a part of Trainer
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.colleen.mod.trainer)

(defvar *dictionaries* (make-hash-table :test 'equalp))

(defmacro define-dictionary (name &body words)
  "Define a dictionary with name NAME.
NAME        --- a symbol or string
WORD        ::= ((TERM+) . (TRANSLATION+))
TERM        --- a string
TRANSLATION --- a string"
  `(setf (gethash ,(string-downcase name) *dictionaries*)
         (mapcar #'(lambda (cons)
                     (make-instance 'word :terms (car cons) :translations (cdr cons)))
                 ',words)))

(defgeneric (setf dictionary) (words name))
(defgeneric (setf term-translations) (translations dictionary term))
(defgeneric (setf term-siblings) (siblings dictionary term))
(defgeneric terms (word-or-dictionary))
(defgeneric translations (word-or-dictionary))
(defgeneric translation-p (word-or-dictionary translation))

(defclass word ()
  ((%terms :initarg :terms :initform () :accessor terms)
   (%translations :initarg :translations :initform () :accessor translations))
  (:documentation "A word."))

(defmethod print-object ((word word) stream)
  (print-unreadable-object (word stream :type T)
    (format stream "~{~s~^, ~}" (terms word)))
  word)

(defun dictionary (name)
  "Retrieve the dictionary with NAME."
  (gethash (string name) *dictionaries*))

(defmethod (setf dictionary) (words name)
  (setf (gethash (string name) *dictionaries*) words))

(defun dictionary-size (name)
  "Retrieve the number of words in the dictionary."
  (list-length (dictionary name)))

(defun copy-dictionary (name)
  "Creates a deep list-copy of the dictionary."
  (copy-list (dictionary name)))

(defun shuffled-dictionary (name)
  "Create a shuffled copy of the dictionary."
  (shuffle (copy-dictionary name)))

(defun find-word (dictionary term)
  "Returns the word data that matches the given term."
  (find term (dictionary dictionary) :key #'terms :test #'(lambda (term terms) (find term terms :test #'string-equal))))

(defun term-translations (dictionary term)
  "Returns the list of translations for the term. See FIND-WORD."
  (translations (find-word dictionary term)))

(defmethod (setf term-translations) (translations dictionary term)
  (setf (translations (find-word dictionary term)) translations))

(defmethod translation-p ((dictionary list) translation)
  "Returns T if the given translations is one of the possible translations for the term. See FIND-WORD."
  (member translation (dictionary dictionary) :key #'translations :test #'(lambda (translation list) (member translation list :test #'string-equal))))

(defmethod translation-p ((word word) translation)
  (member translation (translations word) :test #'string-equal))

(defun term-siblings (dictionary term)
  "Returns the list of sibling terms for the term. See FIND-WORD."
  (terms (find-word dictionary term)))

(defmethod (setf term-siblings) (siblings dictionary term)
  (setf (terms (find-word dictionary term)) siblings))

(defun copy-term-siblings (dictionary term)
  "Create a copy of the list of siblings. See TERM-SIBLINGS."
  (copy-list (term-siblings dictionary term)))

(defun copy-term-translations (dictionary term)
  "Create a copy of the list of translations. See TERM-TRANSLATIONS."
  (copy-list (term-translations dictionary term)))

(defmethod terms ((dictionary list))
  "Returns the list of term-lists of a dictionary."
  (mapcar #'terms (dictionary dictionary)))

(defmethod translations ((dictionary list))
  "Returns the list of translation-lists of a dictionary."
  (mapcar #'translations (dictionary dictionary)))

(defun copy-terms (dictionary)
  "Create a copy of the terms. See TERMS."
  (mapcar #'copy-list (terms dictionary)))

(defun copy-translations (dictionary)
  "Create a copy of the translations. See TRANSLATIONS."
  (mapcar #'copy-list (translations dictionary)))

(defun shuffled-terms (dictionary)
  "Create a shuffled copy of the dictionary's terms."
  (shuffle (copy-terms dictionary)))

(defmacro do-dictionary ((terms translations dictionary &key copy shuffle) &body body)
  "Loop over the dictionary words.
TERMS        --- A symbol bound to each term list in the dictionary.
TRANSLATIONS --- A symbol bound to the corresponding translations list in the dictionary.
DICTIONARY   --- A name used to find the dictionary.
COPY         --- If non-NIL a copy of the dictionary is used as per COPY-DICTIONARY.
SHUFFLE      --- If non-NIL a shuffled copy of the dictionary is used as per SHUFFLED-DICTIONARY.
BODY         ::= form*"
  (let ((word (gensym "WORD")))
    `(loop for ,word in ,(cond (shuffle `(shuffled-dictionary ,dictionary))
                               (copy `(copy-dictionary ,dictionary))
                               (T `(dictionary, dictionary)))
           for ,terms = (terms ,word)
           for ,translations (translations ,word)
           do (progn ,@body))))

(defun add-word (dictionary terms translations)
  (setf (dictionary dictionary)
        (cons (make-instance 'word :terms terms :translations translations) (dictionary dictionary))))

(defun add-terms (dictionary term &rest terms)
  (dolist (new-term terms)
    (pushnew new-term (term-siblings dictionary term) :test #'string-equal)))

(defun add-translations (dictionary term &rest translations)
  (dolist (new-translation translations)
    (pushnew new-translation (term-translations dictionary term) :test #'string-equal)))

(defun remove-word (dictionary term)
  (setf (dictionary dictionary)
        (delete term (dictionary dictionary)
                :test #'(lambda (term word) (member term (terms word) :test #'string-equal))
                :key #'car)))

(defun remove-terms (dictionary term &rest terms)
  (dolist (old-term terms)
    (setf (term-siblings dictionary term)
          (delete old-term (term-siblings dictionary term)
                  :test #'string-equal))))

(defun remove-translations (dictionary term &rest translations)
  (dolist (old-translation translations)
    (setf (term-translations dictionary term)
          (delete old-translation (term-translations dictionary term)
                  :test #'string-equal))))







