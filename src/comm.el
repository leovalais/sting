(defun deserialize-test (test)
  (assert (eql (getf test :tag) :test))
  (destructuring-bind (&key name package description source-info &allow-other-keys)
      test
    (make-sting-test :name name :package package
                     :description description :source-info source-info)))

(defslimefun sting-recieve-tests (tests)
  (setq sting-loaded-tests
        (mapcar #'deserialize-test tests))
  (repaint-buffer))

(defun sting-load-tests ()
  (interactive)
  (slime-eval-async '(sting::send-tests)))


(defun sting-run ()
  (interactive)
  ;; Rewind one character back until a header is found (it has the needed property sting-test).
  ;; That way, we can C-c C-c inside the description of an expanded test and still having it run again.
  (unless sting-loaded-tests
    (error "no tests loaded"))
  (let ((test (loop for p = (point) then (1- p)
                    for test = (get-text-property p 'sting-test)
                    while (and (> p 0)
                               (not test))
                    finally (return test))))
    (if test
        (slime-eval-async `(sting::run (sting::find-test (cl:cons ',(sting-test-package test)
                                                                  ',(sting-test-name test)))))
      (message "no test found at point"))))
