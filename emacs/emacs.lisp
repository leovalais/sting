(in-package :sting)

(defparameter *emacs-client-connected?* nil)
(defparameter *auto-send-test-to-emacs-when* :always
  "Defines when a test is automatically sent to Emacs sting client.
Possible values are:
- :never => disables the feature
- :always => everytime the test is compiled, loaded or dynamically created (default)
- :changed => everytime the test is compiled, loaded or dynamically created, except the first time.")


(defgeneric serialize (object))

(defmethod serialize ((test test))
  (with-slots (name package description source-info) test
    (list :tag :test
          :name (symbol-name name)
          :package (symbol-name package)
          :description description
          :source-info source-info)))

(defmethod serialize ((report imotep))
  (with-slots (test values) report
    (list :tag :pass-report
          :test (serialize test)
          :values (format nil "~S" values))))

(defmethod serialize ((report failure))
  (with-slots (test kind failure-error timeout-seconds) report
    (list :tag :fail-report
          :test (serialize test)
          :kind kind
          :error (format nil "~S" failure-error)
          :timeout-seconds timeout-seconds)))
