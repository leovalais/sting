(defpackage :sting
  (:use :cl :alexandria)
  (:shadow assert)
  (:export test
           report
           imotep
           failure
           *timeout-seconds*
           *auto-run-test-when*

           test-descriptor
           find-test
           remove-test
           clear-tests

           run
           run-all
           run-package
           run-test-with-conditions
           define-test
           define-before
           define-after

           assertion-error
           valued-form
           boolean-assertion-error
           no-error-assertion-error
           wrong-error-assertion-error
           equality-assertion-error
           inequality-assertion-error
           cmp-assertion-error
           assert
           assert-not
           assert-error
           assert-=
           assert-/=
           assert-<
           assert->
           assert-<=
           assert->=

           *emacs-client-connected?*
           *auto-send-test-to-emacs-when*))
