(in-package :sting)

(defmethod add-test-in :after ((tc test-container) (test test))
  (when *auto-run*
    (run-test-with-conditions test)))
