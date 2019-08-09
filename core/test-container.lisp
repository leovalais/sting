(in-package :sting)

(deftype test-descriptor ()
  '(or
    test
    symbol string
    (cons symbol symbol)
    (cons string string)
    (cons package symbol)))

(defun test-hash-table-key (test-descriptor)
  (etypecase test-descriptor
    (test
     (cons (test-package test-descriptor)
           (name test-descriptor)))
    (symbol
     (cons (symbolicate (package-name *package*))
           test-descriptor))
    (string
     (cons (symbolicate (package-name *package*))
           (symbolicate test-descriptor)))
    ((cons symbol symbol)
     test-descriptor)
    ((cons string string)
     (if-let (package (find-package (symbolicate (car test-descriptor))))
       (cons (symbolicate (package-name package))
             (symbolicate (cdr test-descriptor)))
       (error "could not find the package of test ~s" test-descriptor)))
    ((cons package symbol)
     (cons (symbolicate (package-name (car test-descriptor)))
           (cdr test-descriptor)))))


(defclass test-container ()
  ((tests :initform (make-hash-table :test 'equal)
          :reader tc-tests
          :type hash-table)))

(defgeneric find-test-in (test-container test-descriptor)
  (:method ((tc test-container) test-descriptor)
    (declare (type test-descriptor test-descriptor))
    (with-slots (tests) tc
      (let ((key (test-hash-table-key test-descriptor)))
        (gethash key tests)))))

(defgeneric remove-test-in (test-container test-descriptor)
  (:method ((tc test-container) test-descriptor)
    (with-slots (tests) tc
      (let ((key (test-hash-table-key test-descriptor)))
        ;; TODO: also remove generated run method and function
        (remhash key tests)))))

(defgeneric clear-tests-in (test-container)
  (:method ((tc test-container))
    (with-slots (tests) tc
      (setf tests (make-hash-table :test 'equal))
      (values))))

(defgeneric add-test-in (test-container test)
  (:method ((tc test-container) (test test))
    (with-slots (tests) tc
      (let* ((key (test-hash-table-key test))
             (previous-test (gethash key tests)))
        (setf (gethash key tests) test)
        previous-test))))


(defgeneric map-tests (test-container function)
  (:method ((tc test-container) function)
    (let ((results '()))
      (maphash-values (lambda (test)
                        (push (funcall function test) results))
                      (tc-tests tc))
      results)))

(defgeneric iter-tests (test-container procedure)
  (:method ((tc test-container) procedure)
    (maphash-values procedure (tc-tests tc))
    (values)))

(defgeneric tests (test-container)
  (:method ((tc test-container))
    (hash-table-values (tc-tests tc))))

(defparameter *tests* (make-instance 'test-container))

(setf (symbol-function 'find-test) (curry #'find-test-in *tests*))
(setf (symbol-function 'remove-test) (curry #'remove-test-in *tests*))
(setf (symbol-function 'clear-tests) (curry #'clear-tests-in *tests*))
(setf (symbol-function 'add-test) (curry #'add-test-in *tests*))
