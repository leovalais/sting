(defstruct sting-test
  name package description source-info)
(defstruct sting-pass-report
  test values)
(defstruct sting-fail-report
  test kind error timeout-seconds)

(defun sting-cl-test-descriptor (test)
  `(cl:cons ',(sting-test-package test)
            ',(sting-test-name test)))

(defun sting-report-test (report)
  (etypecase report
    (sting-pass-report (sting-pass-report-test report))
    (sting-fail-report (sting-fail-report-test report))))

(defvar sting-loaded-tests (list))
(defvar sting-reports (make-hash-table :test 'equal))
(defvar sting-expanded (make-hash-table))

(defun sting-get-report (test)
  (gethash (cons (sting-test-package test)
                 (sting-test-name test))
           sting-reports))

(defun sting-put-report (report)
  (setf (gethash (cons (sting-test-package (sting-report-test report))
                       (sting-test-name (sting-report-test report)))
                 sting-reports)
        report))

(defun sting-remove-report (test)
  (remhash (cons (sting-test-package test)
                 (sting-test-name test))
           sting-reports))

(defun deserialize-test (test)
  (assert (eql (getf test :tag) :test))
  (destructuring-bind (&key name package description source-info &allow-other-keys) ; the :tag
      test
    (make-sting-test :name name :package package
                     :description description :source-info source-info)))

(defun deserialize-report (report)
  (ecase (getf report :tag)
    (:pass-report
     (destructuring-bind (&key test values &allow-other-keys) report
       (make-sting-pass-report :test (deserialize-test test) :values values)))
    (:fail-report
     (destructuring-bind (&key test kind error timeout-seconds &allow-other-keys)
         report
       (make-sting-fail-report :test (deserialize-test test) :kind kind
                               :error error :timeout-seconds timeout-seconds)))))


(defslimefun sting-recieve-tests (tests)
  (setq sting-loaded-tests
        (mapcar #'deserialize-test tests))
  (setq sting-reports (make-hash-table :test 'equal))
  (setq sting-expanded (make-hash-table))
  (repaint-buffer))

(defslimefun sting-recieve-reports (reports)
  (let ((reports (mapcar #'deserialize-report reports)))
    (mapc #'sting-put-report reports)
    (message (if (some #'sting-fail-report-p reports)
                 "Some tests failed"
               "All tests passed")))
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
        (progn
          (sting-remove-report test) ; remove its report (changes the indicator)
          (repaint-buffer) ; make these changes effective
          (slime-eval-async `(sting::emacs-run-test ,(sting-cl-test-descriptor test))))
      (message "no test found at point"))))
