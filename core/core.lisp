(in-package :sting)


(defmethod clear-tests-in :after ((tc test-container))
  ;; TODO also remove generated functions
  (mapcar (lambda (x) (remove-method #'run x))
          (closer-mop:generic-function-methods #'run)))

(defmethod remove-test-in :after ((tc test-container) test-descriptor)
  ;; TODO remove generated run method and function
  )

(defun run-in-parallel (tests)
  (lparallel:pmapcar #'run tests))

(defun run-all-sequentially ()
  (map-tests *tests* #'run))

(defun run-all (&key (parallel t))
  (if parallel
      (run-in-parallel (tests *tests*))
      (run-all-sequentially)))

(defun run-package-sequentially (package)
  (mapcar #'run (remove package (tests *tests*)
                        :test-not #'eql :key #'test-package)))

(defun run-package (package &key (parallel t))
  (let ((package (etypecase package
                   (package (symbolicate (package-name package)))
                   (keyword (symbolicate (symbol-name package)))
                   (string (symbolicate package))
                   (symbol package))))
    (if parallel
        (run-in-parallel (remove package (tests *tests*)
                                 :test-not #'eql :key #'test-package))
        (run-package-sequentially package))))

(defun run-test-with-conditions (test)
  (let ((report (run test)))
    (when (typep report 'failure)
      (restart-case (error (failure-error report))
        (continue ()
          :report "Let `run-test-with-conditions' finish and return the report.")))
    report))

(defun eval-in-test-runtime (test function)
  (handler-case (trivial-timeout:with-timeout (*timeout-seconds*)
                  (let ((values (multiple-value-list (funcall function))))
                    (make-instance 'imotep :test test :eval-values values)))
    (assertion-error (e)
      (make-instance 'failure
                     :test test
                     :failure-kind :assertion
                     :failure-error e))
    (trivial-timeout:timeout-error (e)
      (make-instance 'failure
                     :test test
                     :failure-kind :timeout
                     :failure-error e
                     :timeout-seconds *timeout-seconds*))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-test (name initargs body)
    (with-gensyms (test test-parameter)
      `(eval-when (:load-toplevel :execute)
         (let ((,test (make-instance 'test
                                     :name ',name
                                     :package (symbolicate (package-name *package*))
                                     ,@initargs)))
           (flet ((test () ,test))
             (declare (ignorable (function test)))
             (defmethod run ((,test-parameter (eql ,test)))
               (eval-in-test-runtime ,test (lambda () ,@body)))
             (defun ,name ()
               (run ,test)))
           (add-test ,test)
           ,test)))))

(defmacro define-test (name &body body-form)
  (multiple-value-bind (initargs body)
      (loop :for b := body-form :then (cdr b)
            :for decl := (car b)
            :while (keywordp (car decl))
            :appending decl :into initargs
            :finally (return (values initargs b)))
    (create-test name initargs body)))
