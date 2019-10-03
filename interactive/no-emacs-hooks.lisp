(in-package :sting)

(setf *auto-run* nil)

(defmethod add-test-in :after ((tc test-container) (test test))
  (when *auto-run*
    (run-test-with-conditions test)))
