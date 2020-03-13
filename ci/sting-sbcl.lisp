#!/usr/local/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setf sb-impl::*default-external-format* :utf-8
      sb-alien::*default-c-string-external-format* :utf-8)

(ql:quickload :sting)
(ql:quickload :lparallel)

(defun read-all (string)
  (with-input-from-string (in string)
    (loop :for value := (read in nil)
          :while value
          :collect value)))

(defun load-systems (systems)
  (dolist (system systems)
    (etypecase system
      ((or string keyword) (ql:quickload system))
      (list (mapcan #'load-systems system))))
  t)

(let ((systems (read-all (or (uiop:getenv "STING_SYSTEMS")
                             "")))
      (N (first (read-all (or (uiop:getenv "STING_THREADS")
                              "8")))))
  (unless systems
    (error "Environment variable STING_SYSTEMS is empty. Aborting."))
  (setf lparallel:*kernel* (lparallel:make-kernel N))
  (load-systems systems))

(let ((reports (sting:run-all)))
  (sting::export-reports reports)
  (let ((fails (count-if-not (lambda (r)
                               (typep r 'sting:success))
                             reports)))
    (unless (zerop fails)
      (sb-posix::exit (1+ fails)))))
