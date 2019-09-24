(in-package :sting)

(defmacro define-dietz-test (name form &rest expected-values)
  `(define-test ,name
     (assert-values (values ,@(quote-list expected-values)) ,form)))

(defmacro with-dietz-deftest (&body body)
  `(macrolet ((deftest (name form &rest expected-values)
                (append (list 'define-dietz-test name form)
                        expected-values)))
     ,@body))
