(in-package :sting)

(defun add-test (test)
  (declare (type test test))
  (let* ((key (test-hash-table-key test))
         (previous-test (gethash key *tests*)))
    (setf (gethash key *tests*) test)
    (when (or (eql *auto-send-test-to-emacs-when* :always)
              (and previous-test
                   (eql *auto-send-test-to-emacs-when* :changed)))
      (send-tests :tests (list test)
                  :wait? t
                  :append? t))
    (when (or (eql *auto-run-test-when* :always)
              (and previous-test
                   (eql *auto-run-test-when* :changed)))
      (if *emacs-client-connected?*
          (emacs-run-test test)
          (run-test-with-conditions test)))
    test))


(defun find-snippet-and-offset-and-file-or-buffer ()
  "Returns a plist (:snippet :offset :file) or (:snippet :offset :buffer)
containing the source information of the form being compiled/loaded at frame
index 1 in SBCL. Requires Swank.
Reference: https://www.snellman.net/blog/archive/2007-12-19-pretty-sbcl-backtraces.html"
  (swank-backend::call-with-debugging-environment
   (lambda ()
     (let* ((data (apply #'append
                         (remove-if #'atom
                                    (swank-backend:frame-source-location 1)))))
       (if (getf data :buffer)
           ;; XXX the returned keyword/value list has an odd number of elements...
           (loop :with buffer :with offset :with snippet
                 :for iter := data :then (cdr iter)
                 :while iter
                 :for k := (car iter)
                 :for v := (cadr iter)
                 :when (eql k :buffer)
                   :do (setf buffer v)
                 :when (eql k :offset)
                   :do (setf offset v)
                 :when (eql k :snippet)
                   :do (setf snippet v)
                 :finally (return (list :buffer buffer :snippet snippet :offset offset)))
           ;; XXX default value for :position to silence SBCL type warning
           (destructuring-bind (&key file (position 0) snippet &allow-other-keys)
               data
             (list :file file :offset (1+ position) :snippet snippet)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-test (name initargs body)
    (with-gensyms (test test-parameter)
      `(eval-when (:load-toplevel :execute)
         (let ((,test (make-instance 'test
                                     :name ',name
                                     :package (symbolicate (package-name *package*))
                                     ,@initargs)))
           #+(and swank sbcl) (setf (source-info ,test)
                                    (find-snippet-and-offset-and-file-or-buffer))
           (flet ((test () ,test))
             (declare (ignorable (function test)))
             (defmethod run ((,test-parameter (eql ,test)))
               (eval-in-test-runtime ,test (lambda () ,@body)))
             (defun ,name ()
               (run ,test)))
           (add-test ,test)
           ,test)))))
