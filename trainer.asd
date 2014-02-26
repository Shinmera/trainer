#|
 This file is a part of Trainer
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.colleen.mod.trainer.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.colleen.mod.trainer.asdf)

(defsystem trainer
  :name "Vocabulary Trainer"
  :version "0.9.6"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A Colleen module to train vocabulary."
  :serial T
  :components ((:file "package")
               (:file "dictionary")
               (:file "program")
               (:file "module")
               (:module "dicts"
                :components ((:file "genki"))))
  :depends-on (:colleen
               :alexandria
               :cl-ppcre))
