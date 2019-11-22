(defpackage :sting
  (:use :cl :alexandria)
  (:shadow assert)
  (:export test
           report
           imotep
           failure
           *timeout-seconds*
           *auto-run*

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

           define-dietz-test
           with-dietz-deftest
           deftest

           assertion-error
           valued-form
           boolean-assertion-error
           no-error-assertion-error
           wrong-error-assertion-error
           equality-assertion-error
           inequality-assertion-error
           cmp-assertion-error
           assert-
           assert-not
           assert-error
           assert-equal
           assert-values
           assert-=
           assert-/=
           assert-<
           assert->
           assert-<=
           assert->=

           *emacs-client-connected?*))
