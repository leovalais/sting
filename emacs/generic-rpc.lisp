(in-package :sting)

(defmacro define-rpc (name lambda-list &body body)
  #+swank `(defun ,name ,lambda-list ,@body)
  #+slynk `(slynk::defslyfun ,name ,lambda-list ,@body))

(defun quote-list (list)
  (declare (type list list))
  (the list
       (mapcar (lambda (x) `',x)
               list)))

(defun funcall-rpc (rpc &rest args)
  (declare (type symbol rpc))
  #+swank (apply #'swank:ed-rpc rpc args)
  #+slynk (slynk:eval-in-emacs `(,rpc ,@(quote-list args)) t))

(defun funcall-rpc-no-wait (rpc &rest args)
  (declare (type symbol rpc))
  #+swank (apply #'swank:ed-rpc-no-wait rpc args)
  #+slynk (slynk:eval-in-emacs `(,rpc ,@(quote-list args)) nil))
