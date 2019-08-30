(in-package :sting)

(defclass fixture ()
  ((fixture :initarg :fixture
            :type function)))

(defclass package-fixture (fixture)
  ((package :initarg :package
            :type package)))

(defclass explicit-fixture (fixture)
  ((tests :initarg :tests
          :type (array (or symbol test) 1))))

(defclass predicate-fixture (fixture)
  ((predicate :initarg :predicate
              :type function)))

(defgeneric fixture-applicable? (fixture test))

(defmethod fixture-applicable? ((fixture fixture) (test test))
  t)

(defmethod fixture-applicable? ((fixture package-fixture) (test test))
  (eql (test-package test)
       (slot-value fixture 'package)))

(defmethod fixture-applicable? ((fixture explicit-fixture) (test test))
  (loop :for t- :in (slot-value fixture 'tests)
          :thereis (etypecase t-
                     (symbol (eql t- (name test)))
                     (test (eql t- test)))))

(defmethod fixture-applicable? ((fixture predicate-fixture) (test test))
  (funcall (slot-value fixture 'predicate)
           test))


(defgeneric apply-fixture (fixture)
  (:method ((fixture fixture))
    (funcall (slot-value fixture 'fixture))
    (values)))



(defclass fixture-container ()
  ((before :initform (make-array 0
                                 :element-type 'fixture-container
                                 :adjustable t
                                 :fill-pointer 0)
           :type (array fixture-filter 1)
           :reader before)
   (after :initform (make-array 0
                                :element-type 'fixture-container
                                :adjustable t
                                :fill-pointer 0)
          :type (array fixture-filter 1)
          :reader after)))

(defgeneric add-fixture (fixture-container fixture kind)
  (:method ((fc fixture-container) (fixture fixture) kind)
    (declare (type (member :before :after) kind))
    (ecase kind
      (:before (vector-push-extend fixture (before fc)))
      (:after (vector-push-extend fixture (after fc))))
    fixture))

(defgeneric run-with-fixtures (test fixture-container function)
  (:method ((test test) (fc fixture-container) function)
    (declare (type function function))
    (loop :for f :across (before fc)
          :if (fixture-applicable? f test)
            :do (apply-fixture f))
    (let ((values (multiple-value-list (funcall function))))
      (loop :for f :across (after fc)
            :if (fixture-applicable? f test)
              :do (apply-fixture f))
      (values-list values))))

(defparameter *fixtures* (make-instance 'fixtures-container))
