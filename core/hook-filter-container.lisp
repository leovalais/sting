(in-package :sting)

(defclass hook-filter-container ()
  ((before :initform (make-array 0
                                 :element-type 'hook-filter-container
                                 :adjustable t
                                 :fill-pointer 0)
           :type (array test-hook-filter 1)
           :reader before)
   (after :initform (make-array 0
                                :element-type 'hook-filter-container
                                :adjustable t
                                :fill-pointer 0)
          :type (array test-hook-filter 1)
          :reader after)))

(defgeneric add-hook (hook-filter-container test-hook-filter kind)
  (:method ((hfc hook-filter-container) (thf test-hook-filter) kind)
    (declare (type (member :before :after) kind))
    (ecase kind
      (:before (vector-push-extend thf (before hfc)))
      (:after (vector-push-extend thf (after hfc))))
    thf))

(defgeneric run-with-hooks (test hook-filter-container function)
  (:method ((test test) (hfc hook-filter-container) function)
    (declare (type function function))
    (loop :for hook :across (before hfc)
          :do (apply-hook hook test))
    (let ((values (multiple-value-list (funcall function))))
      (loop :for hook :across (after hfc)
            :do (apply-hook hook test))
      (values-list values))))

(defparameter *test-hooks* (make-instance 'hook-filter-container))
