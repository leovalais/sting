(in-package :sting)

(locally (declare (optimize (debug 0) (safety 0)
                            (compilation-speed 0)
                            (space 3) (speed 3)))
  (defun inspect-stack-frame ()
    (let* ((data (apply #'append
                        (remove-if #'atom
                                   #+swank (swank-backend:frame-source-location 1)
                                   #+slynk (slynk-backend:frame-source-location 1)))))
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
            (list :file file :offset (1+ position) :snippet snippet)))))
  (defun find-source-info ()
    "Returns a plist (:snippet :offset :file) or (:snippet :offset :buffer)
containing the source information of the form being compiled/loaded at frame
index 1 in SBCL. Requires Swank.
Reference: https://www.snellman.net/blog/archive/2007-12-19-pretty-sbcl-backtraces.html"
    #+swank (swank-backend::call-with-debugging-environment #'inspect-stack-frame)
    #+slynk (slynk-backend::call-with-debugging-environment #'inspect-stack-frame)))
