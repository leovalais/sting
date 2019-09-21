(defun sting-connect ()
  (interactive)
  (message "sting: handshaking Lisp backend...")
  (setq sting-connected?
        (condition-case err
            (funcall-rpc 'sting::handshake)
          (error nil)))
  (if sting-connected?
      (message "sting successfully connected to Lisp backend!")
    (error "sting failed to handshake with Lisp backend"))
  sting-connected?)

(defun sting-clear ()
  (interactive)
  (setq sting-loaded-tests (list)
        sting-reports (make-hash-table :test 'equal)
        sting-expanded (make-hash-table))
  (run-hooks 'sting-update-data-hook))

(defun sting-load-all-tests ()
  (interactive)
  (sting-ensure-state)
  (funcall-rpc-no-wait 'sting::send-tests))

(defun sting-load-package-interactive ()
  (interactive)
  (sting-ensure-state)
  (let* ((packages (funcall-rpc 'sting::get-package-name-list))
         (initial (first packages)))
    (if packages
        (let ((selected (completing-read "Package: " packages nil t initial)))
          (sting-load-package selected))
      (error "no tests loaded in Common Lisp environment"))))

(defun sting-load-package (package)
  (sting-ensure-state)
  (funcall-rpc-no-wait 'sting::send-package package))

(defun sting-ensure-tests-loaded ()
  (unless sting-loaded-tests
    (error "sting: no tests loaded")))

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
  (sting-ensure-tests-loaded)
  ;; Rewind one character back until a header is found (it has the needed property sting-test).
  ;; That way, we can C-c C-c inside the description of an expanded test and still having it run again.
  (if-let (test (sting-backwards-till-property-found 'sting-test))
      (funcall-rpc-no-wait 'sting::emacs-run `(cl:list ,(sting-cl-test-descriptor test)))
    (message "no test found at point")))

(defun sting-run-multiple (tests)
  (sting-ensure-tests-loaded)
  (sting-ensure-state :bring-buffer? :yes)
  (unless tests
    (error "no test provided"))
  (mapc #'sting-mark-test-as-running tests)
  (run-hooks 'sting-update-data-hook)
  (let ((descriptor-list (mapcar #'sting-cl-test-descriptor tests)))
    (funcall-rpc-no-wait 'sting::emacs-run `(cl:list ,@descriptor-list))))

(defun sting-open-source-interactive ()
  (interactive)
  (sting-ensure-tests-loaded)
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

(defun sting-run-all ()
  (interactive)
  (sting-run-multiple sting-loaded-tests))

(defun sting-run-package-interactive ()
  (interactive)
  (let* ((packages (mapcar #'sting-test-package sting-loaded-tests))
         (initial (first packages))
         (selected (completing-read "Package: " packages nil t initial)))
    (sting-run-package selected)))

(defun sting-run-package (package)
  (let ((tests (remove-if-not (lambda (t-)
                                (string= (sting-test-package t-)
                                         package))
                              sting-loaded-tests)))
    (sting-run-multiple tests)))

(defun sting-run-failed ()
  (interactive)
  (let ((tests '()))
    (maphash (lambda (- r)
               (when (sting-fail-report-p r)
                 (push (sting-fail-report-test r)
                       tests)))
             sting-reports)
    (sting-run-multiple tests)))

(defun sting-sort ()
  (interactive)
  (setq sting-loaded-tests (sting-sort-tests sting-loaded-tests))
  (run-hooks 'sting-update-data-hook))
