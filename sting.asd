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
                 (:file "core")
                 (:file "serialization")))
   (:module "emacs"
    :if-feature (:and (:not :sting-no-emacs)
                      (:or :swank :slynk)
                      :sbcl)
    :components ((:file "generic-rpc")
                 (:file "emacs")))
   (:module "hooks"
    :components ((:file "common-hooks" :if-feature (:and (:not :sting-no-emacs)
                                                         (:or :swank :slynk)))
                 (:file "sbcl-hooks" :if-feature (:and (:not :sting-no-emacs)
                                                       (:or :swank :slynk)
                                                       :sbcl))
                 (:file "no-emacs-hooks" :if-feature (:or :sting-no-emacs
                                                          (:not (:or :swank :slynk))))))))
