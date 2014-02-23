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
         ',words))

(defun dictionary (name)
  "Retrieve the dictionary with NAME."
  (gethash (string name) *dictionaries*))

(defgeneric (setf dictionary) (words name))
(defmethod (setf dictionary) (words name)
  (setf (gethash (string name) *dictionaries*) words))

(defun dictionary-size (name)
  "Retrieve the number of words in the dictionary."
  (list-length (dictionary name)))

(defun copy-dictionary (name)
  "Creates a deep list-copy of the dictionary."
  (mapcar #'(lambda (a) (cons (copy-list (car a)) (copy-list (cdr a)))) (dictionary name)))

(defun shuffled-dictionary (name)
  "Create a shuffled copy of the dictionary."
  (shuffle (copy-dictionary name)))

(defun find-word (dictionary term)
  "Returns the word data that matches the given term."
  (find term (dictionary dictionary) :key #'car :test #'string-equal))

(defun term-translations (dictionary term)
  "Returns the list of translations for the term. See FIND-WORD."
  (cdr (find-word dictionary term)))

(defgeneric (setf term-translations) (translations dictionary term))
(defmethod (setf term-translations) (translations dictionary term)
  (setf (cdr (find-word dictionary term)) translations))

(defun translation-p (dictionary term translation)
  "Returns T if the given translations is one of the possible translations for the term. See FIND-WORD."
  (member translation (term-translations dictionary term) :test #'string-equal))

(defun term-siblings (dictionary term)
  "Returns the list of sibling terms for the term. See FIND-WORD."
  (car (find-word dictionary term)))

(defgeneric (setf term-siblings) (siblings dictionary term))
(defmethod (setf term-siblings) (siblings dictionary term)
  (setf (car (find-word dictionary term)) siblings))

(defun copy-term-siblings (dictionary term)
  "Create a copy of the list of siblings. See TERM-SIBLINGS."
  (copy-list (term-siblings dictionary term)))

(defun copy-term-translations (dictionary term)
  "Create a copy of the list of translations. See TERM-TRANSLATIONS."
  (copy-list (term-translations dictionary term)))

(defun terms (dictionary)
  "Returns the list of term-lists of a dictionary."
  (mapcar #'car (dictionary dictionary)))

(defun translations (dictionary)
  "Returns the list of translation-lists of a dictionary."
  (mapcar #'cdr (dictionary dictionary)))

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
  `(loop for (,terms . ,translations) in ,(cond (shuffle `(shuffled-dictionary ,dictionary))
                                                (copy `(copy-dictionary ,dictionary))
                                                (T `(dictionary, dictionary)))
         do (progn ,@body)))

(defun add-word (dictionary terms translations)
  (setf (dictionary dictionary)
        (cons (cons terms translations) (dictionary dictionary))))

(defun add-terms (dictionary term &rest terms)
  (dolist (new-term terms)
    (pushnew new-term (term-siblings dictionary term) :test #'string-equal)))

(defun add-translations (dictionary term &rest translations)
  (dolist (new-translation translations)
    (pushnew new-translation (term-translations dictionary term) :test #'string-equal)))

(defun remove-word (dictionary term)
  (setf (dictionary dictionary)
        (delete term (dictionary dictionary)
                :test #'(lambda (term list) (member term list :test #'string-equal))
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







