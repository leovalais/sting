(in-package :sting)


;;;; Sending data to Emacs

(defun ensure-emacs-connected ()
  (unless *emacs-client-connected?*
    (swank:ed-rpc 'sting-connect-rpc))
  (values))

(defun send-tests (&key (tests *tests*) wait? append?)
  (declare (type (or test-container sequence hash-table) tests)
           (type boolean wait? append?))
  (ensure-emacs-connected)
  (let* ((tests (etypecase tests
                  (test-container (tests tests))
                  (sequence tests)
                  (hash-table (hash-table-values tests))))
         (serialized-tests (map 'list #'serialize tests)))
    (if wait?
        (swank:ed-rpc 'sting-recieve-tests serialized-tests
                      :append? append?)
        (swank:ed-rpc-no-wait 'sting-recieve-tests serialized-tests
                              :append? append?))))

(defun emacs-run-test (test-descriptor)
  (ensure-emacs-connected)
  (if-let (test (find-test test-descriptor))
    (progn
      (swank:ed-rpc-no-wait 'sting-mark-test-as-running-rpc (serialize test))
      (let ((report (run test)))
        (swank:ed-rpc-no-wait 'sting-recieve-reports
                              (list (serialize report)))))
    (error "no test found for descriptor ~s" test-descriptor)))


;;;; Recieveing data from Emacs

(defun handshake ()
  "Hanshakes with sting Emacs client. Returns T to signal that CL connection job
has succeeded. Fails if another (perhaps the same) sting client was connected,
signals an error and sends NIL to the client which initiated this handshake."
  (flet ((ok ()
           (setf *emacs-client-connected?* t)
           t))
    (cond
      (*emacs-client-connected?*
       (restart-case (error "A sting client is already connected.")
         (still-connect ()
           :report "Ignore the possible redundancy and proceed anyway (have you multiple buffers with sting-mode in?)"
           (ok))))
      (t (ok)))))
