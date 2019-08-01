(in-package :sting)

;;;; TODO: conditional compilation

(defparameter *sting-client-connected?* nil)

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
  "Hanshakes with sting Emacs client. Returns T to signal that CL connection job
has succeeded. Fails if another (perhaps the same) sting client was connected,
signals an error and sends NIL to the client which initiated this handshake."
  (flet ((ok ()
           (setf *sting-client-connected?* t)
           t))
    (cond
      (*sting-client-connected?*
       (restart-case (error "A sting client is already connected.")
         (still-connect ()
           :report "Ignore the possible redundancy and proceed anyway (have you multiple buffers with sting-mode in?)"
           (ok))))
      (t (ok)))))

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
