(in-package :sting)

(defmethod add-test-in :before ((tc test-container) (test test))
  (let* ((key (test-hash-table-key test))
         (previous-test (gethash key (tc-tests tc)))
         (test-changed? (and previous-test
                             (not (eql previous-test test)))))
    (when (or (eql *auto-send-test-to-emacs-when* :always)
              (and test-changed?
                   (eql *auto-send-test-to-emacs-when* :changed)))
      (send-tests :tests (list test)
                  :wait? t
                  :append? t))
    (when (or (eql *auto-run-test-when* :always)
              (and test-changed?
                   (eql *auto-run-test-when* :changed)))
      (if *emacs-client-connected?*
          (emacs-run-test test)
          (run-test-with-conditions test)))))
