(defvar sting-connected? nil)
(defvar sting-action-at-connection nil
  "Defines what to do when sting is successfully connected to slime.
Its values can be:
- nil (default) => do nothing
- :load => loads the tests
- :load-and-run => loads and run the tests.")
(defvar sting-loaded-tests (list))
(defvar sting-reports (make-hash-table :test 'equal))
(defvar sting-expanded (make-hash-table))
(defvar sting-update-data-hook nil
  "Hook called whenever core data has changed. Should update the view buffer.")
(defvar sting-ensure-state-hook nil
  "Hook called whenever the validity of sting's state needs to be ensured.")

(defvar sting-locked? nil)

(cl-defun sting-lock (&optional (owner t) reentrant)
  (message "sting: locked by %s%s" owner (if reentrant " (reentrant)" ""))
  (if (and sting-locked?
           (not reentrant))
      (error "sting is locked. call (sting-unlock) or wait till operation finishes. previously locked owner: %s" sting-locked?)
    (setq sting-locked? owner)))

(cl-defun sting-unlock (&optional can-fail?)
  (message "sting: unlocked%s" (if can-fail? " (failures permitted)" ""))
  (if (or sting-locked?
          can-fail?)
      (setq sting-locked? nil)
    (error "sting is not locked. this is probably due to an unstable state or is a bug. please report it on github.com/leovalais/sting.")))

(defmacro with-sting-lock (owner &rest body)
  `(unwind-protect (progn
                     (sting-lock ,(or owner t))
                     ,@body)
     (sting-unlock)))

(defstruct sting-test
  name package description source-info)
(defstruct sting-pass-report
  test values)
(defstruct sting-fail-report
  test kind error timeout-seconds)

(defun sting-test= (a b)
  (and (typep a 'sting-test)
       (typep b 'sting-test)
       (equal (sting-test-name a)
              (sting-test-name b))
       (equal (sting-test-package a)
              (sting-test-package b))))

(defun sting-cl-test-descriptor (test)
  `(cl:cons ',(sting-test-package test)
            ',(sting-test-name test)))

(defun sting-report-test (report)
  (etypecase report
    (sting-pass-report (sting-pass-report-test report))
    (sting-fail-report (sting-fail-report-test report))))

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

(defun sting-mark-test-as-running (test)
  (sting-remove-report test)
  (setf (gethash (cons (sting-test-package test)
                       (sting-test-name test))
                 sting-reports)
        :running))

(defun sting-sort-tests ()
  (setq sting-loaded-tests
        (sort sting-loaded-tests
              (lambda (t1 t2) ; returns whether t1 < t2
                (let ((pt1 (sting-test-package t1))
                      (pt2 (sting-test-package t2))
                      (nt1 (sting-test-name t1))
                      (nt2 (sting-test-name t2)))
                  (cond
                   ((string-lessp pt1 pt2) t)
                   ((string-equal pt1 pt2) (string-lessp nt1 nt2))))))))



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


(defun sting-ensure-state (&rest keys)
  (unless sting-connected?
    (sting-connect))
  (run-hook-with-args 'sting-ensure-state-hook keys)
  (values))

(defslimefun sting-recieve-tests (tests &key append?)
  (with-sting-lock sting-recieve-tests
   (sting-ensure-state)
   (let ((tests (mapcar #'deserialize-test tests)))
     (if append?
         (dolist (t- tests)
           (pushnew t- sting-loaded-tests :test #'sting-test=))
       (setq sting-loaded-tests tests)
       (setq sting-reports (make-hash-table :test 'equal))
       (setq sting-expanded (make-hash-table))))
   (sting-sort-tests)
   (run-hooks 'sting-update-data-hook)))

(defslimefun sting-recieve-reports (reports)
  (sting-ensure-state :bring-buffer? :yes)
  (let ((reports (mapcar #'deserialize-report reports)))
    (mapc #'sting-put-report reports)
    (message (if (some #'sting-fail-report-p reports)
                 "Some tests failed"
               "All tests passed")))
  (run-hooks 'sting-update-data-hook)
  (sting-unlock))

(defslimefun sting-mark-test-as-running-rpc (test)
  (sting-ensure-state :bring-buffer? :yes)
  (sting-lock 'sting-mark-test-as-running-rpc)
  (sting-mark-test-as-running (deserialize-test test))
  (run-hooks 'sting-update-data-hook))

(defslimefun sting-connect-rpc ()
  (with-sting-lock sting-connect-rpc
   (sting-connect)))

(defun sting-connect ()
  (interactive)
  (with-sting-lock sting-connect
   (message "sting: handshaking slime...")
   (setq sting-connected?
         (condition-case err
             (slime-eval '(sting::handshake))
           (error nil)))
   (if sting-connected?
       (message "sting successfully connected to slime!")
     (error "sting failed to handshake with slime"))
   sting-connected?))

(defun sting-load-tests ()
  (interactive)
  (sting-lock 'sting-load-tests)
  (sting-ensure-state)
  (slime-eval-async '(sting::send-tests)))

(defun sting-backwards-till-property-found (property)
  "Starts at `point' and moves one character backwards until a character with `property' is found.
Returns the value of that property for that character."
  (loop for p = (point) then (1- p)
        while (/= p 0)
        for value = (get-text-property p property)
        until value
        finally (return value)))

(defun sting-run ()
  (interactive)
  (unless sting-loaded-tests
    (error "no tests loaded"))
  ;; Rewind one character back until a header is found (it has the needed property sting-test).
  ;; That way, we can C-c C-c inside the description of an expanded test and still having it run again.
  (if-let (test (sting-backwards-till-property-found 'sting-test))
      (progn
        (sting-mark-test-as-running test) ; remove its report and changes the indicator
        (run-hooks 'sting-update-data-hook) ; make these changes effective
        (slime-eval-async `(sting::emacs-run-test ,(sting-cl-test-descriptor test))))
      (message "no test found at point")))

(defun sting-open-source-interactive ()
  (interactive)
  (unless sting-loaded-tests
    (error "no tests loaded"))
  (if-let (test (sting-backwards-till-property-found 'sting-test))
      (sting-open-source test)
    (message "no test found at point")))

(defun sting-open-source (test)
  (if-let (source-info (sting-test-source-info test))
      (progn
        (if-let (buffer (getf source-info :buffer))
            (pop-to-buffer buffer)
          (find-file-other-window (getf source-info :file)))
        (goto-char (getf source-info :offset)))
    (error "no source information found for test %s" test)))
