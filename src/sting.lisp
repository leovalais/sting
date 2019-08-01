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
                :type string)
   (source-info :initarg :source-info
                :accessor source-info
                :type list)))

(defparameter *tests* (make-hash-table :test 'equal))
(defparameter *timeout-seconds* 2)

(defun remove-all-tests ()
  (mapcar (lambda (x) (remove-method #'run x))
          (closer-mop:generic-function-methods #'run))
  (setf *tests* (make-hash-table :test 'equal))
  (values))

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

(defun remove-test (test-descriptor)
  (let ((key (test-hash-table-key test-descriptor)))
    ;; TODO: also remove generated run method and function
    (remhash key *tests*)))

(defun add-test (test)
  (declare (type test test))
  (let ((key (test-hash-table-key test)))
    (when (gethash key *tests*)
      (warn "redefinition of test ~a" key))
    (setf (gethash key *tests*) test)))

(defun find-test (test-descriptor)
  (let ((key (test-hash-table-key test-descriptor)))
    (gethash key *tests*)))

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
                  :type condition)
   (timeout-seconds :initarg :timeout-seconds
                    :initform nil
                    :reader timeout-seconds
                    :type (or null number))))

(defgeneric run (test)
  (:documentation "Executes the content of the given `test'.
`test' can be:
- an instance of a subtype of `sting:test'
- an EQL specializer on the name of the test

It returns a `sting:report' (see `sting:imotep' and `sting:failure')."))


#+(and swank sbcl)
(defun find-snippet-and-offset-and-file-or-buffer ()
  "Returns a plist (:snippet :offset :file) or (:snippet :offset :buffer)
containing the source information of the form being compiled/loaded at frame
index 1 in SBCL. Requires Swank.
Reference: https://www.snellman.net/blog/archive/2007-12-19-pretty-sbcl-backtraces.html"
  (swank-backend::call-with-debugging-environment
   (lambda ()
     (let* ((data (apply #'append
                         (remove-if #'atom
                                    (swank-backend:frame-source-location 1)))))
       (if (getf data :buffer)
           ;; XXX the returned keyword/value list has an odd number of elements...
           (loop :with buffer :with offset :with snippet
                 :for iter := data :then (cdr iter)
                 :while iter
                 :for k := (car iter)
                 :for v := (cadr iter)
                 :when (eql k :buffer)
                   :do (setf buffer v)
                 :when (eql k :offset)
                   :do (setf offset v)
                 :when (eql k :snippet)
                   :do (setf snippet v)
                 :finally (return (list :buffer buffer :snippet snippet :offset offset)))
           ;; XXX default value for :position to silence SBCL type warning
           (destructuring-bind (&key file (position 0) snippet &allow-other-keys)
               data
             (list :file file :offset (1+ position) :snippet snippet)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-test (name initargs body)
    (with-gensyms (test test-parameter)
      `(eval-when (:load-toplevel :execute)
         (let ((,test (make-instance 'test
                                     :name ',name
                                     :package (symbolicate (package-name *package*))
                                     ,@initargs)))
           #+(and swank sbcl) (setf (source-info ,test)
                                    (find-snippet-and-offset-and-file-or-buffer))
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

(setf lparallel:*kernel* (lparallel:make-kernel 12))

(defmethod run :before (test)
  (format t "running test ~a~%" (name test)))


;;; FIXME loading a region messes with the :snippet and the :offset

(define-test t1
  (:description "mdr")
  (assert-= 1 2))

(define-test t2
  (assert-not (sleep 10)))

(define-test t3
  (:description "t3 is the new t2")
  (assert-< 1 2))
