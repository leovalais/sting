(in-package :sting)

;;;; TODO: conditional compilation

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

(defun handshake ()
  t)

(defun send-tests ()
  (swank:ed-rpc-no-wait 'sting-recieve-tests
                        (mapcar #'serialize
                                (hash-table-values *tests*))))

(defun emacs-run-test (test-descriptor)
  (if-let (test (find-test test-descriptor))
    (let ((report (run test)))
      (swank:ed-rpc-no-wait 'sting-recieve-reports
                            (list (serialize report))))
    (error "no test found for descriptor ~s" test-descriptor)))
