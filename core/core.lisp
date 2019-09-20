(in-package :sting)

(defun run (test)
  (with-slots (name) test
    (if (fboundp name)
        (run-with-fixtures test *fixtures*
                           (symbol-function (name test)))
        (error "test ~S has no associated function named ~S" test name))))

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
                   (package package)
                   ((or string symbol) (find-package package)))))
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
  (declare (type function function))
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


(defparameter *define-test-hooks* (make-hash-table))
(defun apply-define-test-hook (hook &rest args)
  (mapcar (lambda (f)
            (declare (type function f))
            (apply f args))
          (gethash hook *define-test-hooks*)))
(defmacro define-define-test-hook (hook lambda-list &body body)
  `(eval-when (:load-toplevel :execute)
     (push (lambda ,lambda-list ,@body)
           (gethash ,hook *define-test-hooks*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-test (name initargs body)
    (with-gensyms (test)
      `(eval-when (:load-toplevel :execute)
         (let ((,test (make-instance 'test
                                     :name ',name
                                     ,@initargs)))
           (flet ((test () ,test))
             (declare (ignorable (function test)))
             ,@(apply-define-test-hook :before-test-defun test :name name :initargs initargs)
             (defun ,name ()
               (eval-in-test-runtime ,test (lambda () ,@body)))
             ,@(apply-define-test-hook :after-test-defun test :name name :initargs initargs)
             ,@(apply-define-test-hook :before-add-test test :name name :initargs initargs)
             (add-test ,test)
             ,@(apply-define-test-hook :after-add-test test :name name :initargs initargs))
           ,test)))))

(defmacro define-test (name &body body-form)
  (multiple-value-bind (initargs body)
      (loop :for b := body-form :then (cdr b)
            :for decl := (car b)
            :while (keywordp (car decl))
            :appending decl :into initargs
            :finally (return (values initargs b)))
    (create-test name initargs body)))


(defun parse-fixture-decl (fixture-decl)
  (etypecase fixture-decl
    ((member nil t) '(make-instance 'fixture))
    (symbol `(make-instance 'explicit-fixture
                            :tests (list ,fixture-decl)))
    (list
     (ecase (first fixture-decl)
       (:package `(make-instance 'package-fixture
                                 :package (find-package ,(second fixture-decl))))
       (:predicate `(make-instance 'predicate-fixture
                                   :predicate #',(second fixture-decl)))
       (:list `(make-instance 'explicit-fixture
                              :tests (list ,@(mapcar (lambda (s)
                                                       `(quote ,s))
                                                     (rest fixture-decl)))))))))

(defmacro define-before (fixture &body body)
  `(set-fixture *fixtures*
                (,@(parse-fixture-decl fixture) :fixture (lambda () ,@body))
                :before))

(defmacro define-after (fixture &body body)
  `(set-fixture *fixtures*
                (,@(parse-fixture-decl fixture) :fixture (lambda () ,@body))
                :after))
