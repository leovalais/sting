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


(defparameter *tests* (make-hash-table :test 'equal))
(defparameter *timeout-seconds* 2)
(defparameter *auto-run-test-when* :changed
  "Defines when a test is automatically run.
Possible values are:
- :never => disables the feature
- :always => everytime the test is compiled, loaded or dynamically created
- :changed => everytime the test is compiled, loaded or dynamically created, except the first time (default)")


(defgeneric run (test)
  (:documentation "Executes the content of the given `test'.
`test' can be:
- an instance of a subtype of `sting:test'
- an EQL specializer on the name of the test
It returns a `sting:report' (see `sting:imotep' and `sting:failure')."))
