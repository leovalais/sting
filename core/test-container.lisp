(in-package :sting)

(deftype test-descriptor ()
  '(or
    test
    symbol string
    (cons string string)
    (cons package symbol)))

(defun test-hash-table-key (test-descriptor)
  (etypecase test-descriptor
    (test   (name test-descriptor))
    (symbol test-descriptor)
    (string (intern test-descriptor *package*))
    ((or (cons string string)
         (cons package symbol))
     (intern (cdr test-descriptor)
             (car test-descriptor)))))


(defclass test-container ()
  ((tests :initform (make-hash-table)
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
        (remhash key tests)))))

(defgeneric clear-tests-in (test-container)
  (:method ((tc test-container))
    (with-slots (tests) tc
      (setf tests (make-hash-table))
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

(defgeneric test-package-list (test-container)
  (:method ((tc test-container))
    (remove-duplicates (mapcar #'test-package
                               (tests tc)))))

(defgeneric tests-of-package (test-container package)
  (:method ((tc test-container) package)
    (let ((package (etypecase package
                     (package package)
                     ((or string symbol) (find-package package)))))
      (remove-if-not (lambda (t-)
                       (eql (test-package t-) package))
                     (tests tc)))))


(defparameter *tests* (make-instance 'test-container))

(setf (symbol-function 'find-test) (curry #'find-test-in *tests*))
(setf (symbol-function 'remove-test) (curry #'remove-test-in *tests*))
(setf (symbol-function 'clear-tests) (curry #'clear-tests-in *tests*))
(setf (symbol-function 'add-test) (curry #'add-test-in *tests*))
