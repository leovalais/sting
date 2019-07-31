(in-package :sting)

(defclass test ()
  ((name :initarg :name
         :initform (error "a test must have a name")
         :reader name
         :type (and symbol (not keyword)))
   (package :initarg :package
            :reader test-package
            :type symbol)
   (description :initarg :description
                :initform ""
                :accessor description
                :type string)))

(defparameter *tests* (make-hash-table :test 'equal))
(defparameter *timeout-seconds* 5)

(defun remove-test (test)
  (let ((key (etypecase test
               (test
                (cons (test-package test)
                      (name test)))
               (symbol
                (cons (symbolicate (package-name *package*))
                      test))
               ((cons symbol symbol)
                test)
               ((cons package symbol)
                (cons (symbolicate (package-name (car test)))
                      (cdr test))))))
    (remhash key *tests*)))

(defun add-test (test)
  (let ((key (cons (test-package test)
                   (name test))))
    (when (gethash key *tests*)
      (warn "redefinition of test ~a" key))
    (setf (gethash key *tests*) test)))

(defclass report ()
  ((test :initarg :test
         :initform (error "a report must have a test")
         :reader report-test
         :type test)))

(defclass imotep (report)
  ((values :initarg :eval-values
           :reader eval-values)))

(defclass failure (report)
  ((kind :initarg :failure-kind
         :reader failure-kind
         :type (member :assertion :timeout))
   (failure-error :initarg :failure-error
                  :reader failure-error
                  :type failure-error)))

(defgeneric run (test)
  (:documentation "Executes the content of the given `test'.
* `test' can be:
  - an instance of a subtype of `sting:test'
  - an EQL specializer on the name of the test

It returns a `sting:report' (see `sting:imotep' and `sting:failure')."))


(defun create-test (name initargs body)
  (with-gensyms (test test-parameter)
    `(let ((,test (make-instance 'test
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
       ,test)))

(defmacro define-test (name &body body-form)
  (multiple-value-bind (initargs body)
      (loop :for b := body-form :then (cdr b)
            :for decl := (car b)
            :while (keywordp (car decl))
            :appending decl :into initargs
            :finally (return (values initargs b)))
    (create-test name initargs body)))

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
                     :failure-error e))))


;; (defun run-all ()
;;   (dolist (run-method (closer-mop:generic-function-methods #'run))


(mapcar (lambda (x) (remove-method #'run x))
        (closer-mop:generic-function-methods #'run))

(defmethod run :before (test)
  (format t "running test ~a~%" (name test)))

(define-test t1
  (:description "mdr")
  (assert-= 1 2))

(define-test t2
  (:description "t2")
  (assert-< 1 2))

(define-test t3
  (assert-not (sleep 10)))
