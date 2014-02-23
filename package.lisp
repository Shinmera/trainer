#|
 This file is a part of Trainer
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.colleen.mod.trainer
  (:nicknames #:trainer)
  (:use #:cl #:colleen #:events #:alexandria)
  ;; dictionary.lisp
  (:export
   #:*dictionaries*
   #:define-dictionary
   #:dictionary
   #:dictionary-size
   #:copy-dictionary
   #:shuffled-dictionary
   #:find-word
   #:term-translations
   #:translation-p
   #:term-siblings
   #:copy-term-siblings
   #:copy-term-translations
   #:terms
   #:translations
   #:copy-terms
   #:copy-translations
   #:shuffled-terms
   #:do-dictionary
   #:add-word
   #:add-terms
   #:add-translations
   #:remove-word
   #:remove-terms
   #:remove-translations)
  ;; module.lisp
  (:export))
