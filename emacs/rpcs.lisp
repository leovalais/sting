(in-package :sting)

(defun the-test (test-descriptor)
  (typecase test-descriptor
    ;; this typecheck is necessary because of `add-test-in'
    ;; :before method which triggers the execution: the new
    ;; is not yet added in `*tests*', therefore,
    ;; `find-test' would be likely to return the old version
    ;; of that test.
    (test test-descriptor)
    (t (find-test test-descriptor))))



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

(defun ensure-emacs-connected ()
  (unless *emacs-client-connected?*
    (swank:ed-rpc 'sting-connect-rpc))
  (values))

(defun send-tests (&key (tests *tests*) wait?)
  (declare (type (or test-container sequence hash-table) tests)
           (type boolean wait?))
  (ensure-emacs-connected)
  (let* ((tests (etypecase tests
                  (test-container (tests tests))
                  (sequence tests)
                  (hash-table (hash-table-values tests))))
         (serialized-tests (map 'list #'serialize tests)))
    (if wait?
        (swank:ed-rpc 'sting-recieve-tests serialized-tests)
        (swank:ed-rpc-no-wait 'sting-recieve-tests serialized-tests))))

(defun emacs-run (test-descriptors)
  (ensure-emacs-connected)
  (let* ((tests (mapcar #'the-test test-descriptors))
         (serialized-tests (mapcar #'serialize tests)))
    (if tests
        (progn
          (swank:ed-rpc-no-wait 'sting-mark-tests-as-running-rpc serialized-tests)
          (let* ((reports (run-in-parallel tests))
                 (serialized-reports (mapcar #'serialize reports)))
            (swank:ed-rpc-no-wait 'sting-recieve-reports serialized-reports)))
        (error "no test found for descriptors ~s" test-descriptors))))

(defun get-package-name-list ()
  (mapcar #'symbol-name (test-package-list *tests*)))

(defun send-package (package-name)
  (send-tests :tests (tests-of-package *tests* package-name)))
