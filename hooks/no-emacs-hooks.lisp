(in-package :sting)

(defmethod add-test-in :before ((tc test-container) (test test))
  (let* ((key (test-hash-table-key test))
         (previous-test (gethash key (tc-tests tc))))
    (when (or (eql *auto-run-test-when* :always)
              (and previous-test
                   (not (eql previous-test test))
                   (eql *auto-run-test-when* :changed)))
      (run-test-with-conditions test))))
