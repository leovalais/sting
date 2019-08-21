(in-package :sting)

(defmethod add-test-in :before ((tc test-container) (test test))
  (let* ((key (test-hash-table-key test))
         (previous-test (gethash key (tc-tests tc)))
         (test-changed? (and previous-test
                             (not (eql previous-test test)))))
    (when (or (eql *auto-send-test-to-emacs-when* :always)
              (and test-changed?
                   (eql *auto-send-test-to-emacs-when* :changed)))
      (send-tests :tests (list test)
                  :wait? t
                  :append? t))
    (when (or (eql *auto-run-test-when* :always)
              (and test-changed?
                   (eql *auto-run-test-when* :changed)))
      (if *emacs-client-connected?*
          (emacs-run (list test))
          (run-test-with-conditions test)))))


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

(define-define-test-hook :before-run-defmethod (test-gensym &key &allow-other-keys)
  `(setf (source-info ,test-gensym)
         (find-snippet-and-offset-and-file-or-buffer)))
