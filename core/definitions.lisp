(in-package :sting)

(defclass test ()
  ((name :initarg :name
         :initform (error "a test must have a name")
         :reader name
         :type (and symbol (not keyword)))
   (description :initarg :description
                :initform ""
                :accessor description
                :type string)
   (source-info :initarg :source-info
                :accessor source-info
                :type list)))

(defgeneric test-package (test)
  (:method ((test test))
    (find-package (symbol-package (name test)))))

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


(defparameter *timeout-seconds* 2)
(defparameter *auto-run* t
  "Defines whether a test is automatically executed whenever it is compiled
whithin SLIME/SLY.")
