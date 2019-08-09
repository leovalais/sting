(defpackage :sting
  (:use :cl :alexandria)
  (:shadow assert)
  (:export report
           imotep
           failure
           *timeout-seconds*
           *auto-run-test-when*

           find-test
           remove-test
           remove-all-tests
           add-test
           run-all
           run-package
           run-test-with-conditions
           define-test

           assertion-error
           assert
           assert-not
           assert-error
           define-assertion
           assert-=
           assert-/=
           assert-<
           assert->
           assert-<=
           assert->=

           *auto-send-test-to-emacs-when*
           serialize))
