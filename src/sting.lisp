(in-package :sting)

(defparameter *timeout-seconds* 5)

(defclass test ()
  ((name :initarg :name
         :initform (error "a test must have a name")
         :reader name
         :type (and symbol (not keyword)))
   (description :initarg :description
                :initform ""
                :accessor description
                :type string)))

(defclass group () nil)

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

(defgeneric run (test group)
  (:documentation "Executes the content of the `test' in the given `group'.
* `test' can be:
  - an instance of a subtype of `sting:test'
  - an EQL specializer on the name of the test
* `group' is an instance of either `sting:group' or one of its subclasses.

It returns a `sting:report' (see `sting:imotep' and `sting:failure')."))


;;; TODO actually implement group logic...
(defun create-test (name group initargs body)
  (with-gensyms (test)
    `(let ((,test (make-instance 'test :name ',name ,@initargs)))
       (flet ((test () ,test))
         (declare (ignorable (function test)))
         (defmethod run ((test (eql ,test)) group)
           (eval-in-test-runtime ,test (lambda () ,@body)))
         ;; (defmethod run ((test (eql ',name)) group)
         ;;   (run ,test ',group))
         (defun ,(if group
                     (symbolicate group "/" name)
                     name)
             ()
           (run ,test ',group))))))

(defmacro define-test (name-form &body body-form)
  (multiple-value-bind (name group)
      (trivia:match name-form
        ((type symbol) (values name-form nil))
        ((list name) (values name nil))
        ((list group name) (values name group)))
    (multiple-value-bind (initargs body)
        (loop :for b := body-form :then (cdr b)
              :for decl := (car b)
              :while (keywordp (car decl))
              :appending decl :into initargs
              :finally (return (values initargs b)))
      (create-test name group initargs body))))

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

(defmethod run :before (test _)
  (format t "running test ~a~%" (name test)))

(define-test (stp t1)
  (:description "mdr")
  (assert-= 1 2))

(define-test t2
  (:description "t2")
  (assert-< 1 2))

(define-test t3
  (assert-not (sleep 10)))
