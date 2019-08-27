(in-package :sting)

(defclass test-hook-filter ()
  ((hook :initarg :hook
         :type function)))

(defclass package-filter (test-hook-filter)
  ((package :initarg :package
            :type package)))

(defclass explicit-tests-filter (test-hook-filter)
  ((tests :initarg :tests
          :type (array (or symbol test) 1))))

(defclass predicate-filter (test-hook-filter)
  ((predicate :initarg :predicate
              :type function)))

(defgeneric can-apply-hook? (filter test))

(defmethod can-apply-hook? ((filter test-hook-filter) (test test))
  t)

(defmethod can-apply-hook? ((filter package-filter) (test test))
  (eql (test-package test)
       (slot-value filter 'package)))

(defmethod can-apply-hook? ((filter explicit-tests-filter) (test test))
  (loop :for t- :in (slot-value filter 'tests)
          :thereis (etypecase t-
                     (symbol (eql t- (name test)))
                     (test (eql t- test)))))

(defmethod can-apply-hook? ((filter predicate-filter) (test test))
  (funcall (slot-value filter 'predicate)
           test))


(defgeneric apply-hook (filter test)
  (:method ((filter test-hook-filter) (test test))
    (if (can-apply-hook? filter test)
        (progn
          (funcall (slot-value filter 'hook))
          t)
        nil)))
