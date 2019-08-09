(in-package :sting)

(defun test-hash-table-key (test-descriptor)
  (etypecase test-descriptor
    (test
     (cons (test-package test-descriptor)
           (name test-descriptor)))
    (symbol
     (cons (symbolicate (package-name *package*))
           test-descriptor))
    (string
     (cons (symbolicate (package-name *package*))
           (symbolicate test-descriptor)))
    ((cons symbol symbol)
     test-descriptor)
    ((cons string string)
     (if-let (package (find-package (symbolicate (car test-descriptor))))
       (cons (symbolicate (package-name package))
             (symbolicate (cdr test-descriptor)))
       (error "could not find the package of test ~s" test-descriptor)))
    ((cons package symbol)
     (cons (symbolicate (package-name (car test-descriptor)))
           (cdr test-descriptor)))))

(defun find-test (test-descriptor)
  (let ((key (test-hash-table-key test-descriptor)))
    (gethash key *tests*)))

(defun remove-test (test-descriptor)
  (let ((key (test-hash-table-key test-descriptor)))
    ;; TODO: also remove generated run method and function
    (remhash key *tests*)))

(defun remove-all-tests ()
  (mapcar (lambda (x) (remove-method #'run x))
          (closer-mop:generic-function-methods #'run))
  (setf *tests* (make-hash-table :test 'equal))
  (values))

(defun add-test (test)
  (declare (type test test))
  (let* ((key (test-hash-table-key test))
         (previous-test (gethash key *tests*)))
    (setf (gethash key *tests*) test)
    (when (or (eql *auto-run-test-when* :always)
              (and previous-test
                   (eql *auto-run-test-when* :changed)))
      (run-test-with-conditions test)))
  test)


(defun run-in-parallel (tests)
  (lparallel:pmapcar #'run tests))

(defun run-all-sequentially ()
  (let ((reports '()))
    (maphash-values (lambda (t-)
                      (push (run t-) reports))
                    *tests*)
    reports))

(defun run-all (&key (parallel t))
  (if parallel
      (run-in-parallel (hash-table-values *tests*))
      (run-all-sequentially)))

(defun run-package-sequentially (package)
  (let ((reports '()))
    (maphash-values (lambda (t-)
                      (when (eql (test-package t-) package)
                        (push (run t-) reports)))
                    *tests*)
    reports))

(defun run-package (package &key (parallel t))
  (let ((package (etypecase package
                   (package (symbolicate (package-name package)))
                   (keyword (symbolicate (symbol-name package)))
                   (string (symbolicate package))
                   (symbol package))))
    (if parallel
        (run-in-parallel (remove package (hash-table-values *tests*)
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
