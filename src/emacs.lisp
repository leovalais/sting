(in-package :sting)

(defgeneric serialize (object))

(defmethod serialize ((test test))
  (with-slots (name package description source-info) test
    (list :tag :test
          :name (symbol-name name)
          :package (symbol-name package)
          :description description
          :source-info source-info)))

(defun send-tests ()
  (swank:ed-rpc-no-wait 'sting-recieve-tests
                        (mapcar #'serialize
                                (hash-table-values *tests*))))
