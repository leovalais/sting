;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :sting
  :name "sting"
  :version "0.1"
  :maintainer "Léo Valais"
  :author "Léo Valais"
  :licence "MIT"
  :description "A Common Lisp testing library with Emacs TDD support."

  :depends-on (:alexandria
               :trivial-timeout
               :lparallel
               :closer-mop)

  :serial t
  :components
  ((:file "package")
   (:module "core"
    :components ((:file "assertions")
                 (:file "definitions")
                 (:file "fixtures")
                 (:file "test-container")
                 (:file "core")))
   (:module "emacs"
    :if-feature (:and :swank :sbcl)
    :components ((:file "emacs")
                 (:file "rpcs")))
   (:module "hooks"
    :components ((:file "sbcl-hooks" :if-feature (:and :sbcl :swank))
                 (:file "no-emacs-hooks" :if-feature (:not :swank))))))
