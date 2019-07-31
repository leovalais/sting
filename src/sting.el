(defstruct sting-test
  name package description source-info)

(defstruct sting-pass-report
  values)

(defstruct sting-fail-report
  kind error timeout-seconds)

(defvar sting-loaded-tests (list (make-sting-test :name 't1 :package 'cl-user
                                                  :description "The first test"
                                                  :source-info '())
                                 (make-sting-test :name 't2 :package 'cl-user
                                                  :description "The second test"
                                                  :source-info '())
                                 (make-sting-test :name 't3 :package 'cl-user
                                                  :description "The third test"
                                                  :source-info '())
                                 (make-sting-test :name 't4 :package 'cl-user
                                                  :description "The fourth test"
                                                  :source-info '())))

(defvar sting-reports (make-hash-table))
(progn
  (setf (gethash (first sting-loaded-tests) sting-reports) (make-sting-pass-report :values '(42)))
  (setf (gethash (third sting-loaded-tests) sting-reports) (make-sting-fail-report :kind :assertion))
  (setf (gethash (fourth sting-loaded-tests) sting-reports) (make-sting-fail-report :kind :timeout :timeout-seconds 5)))

(defvar sting-expanded (make-hash-table))
(progn
  (setf (gethash (third sting-loaded-tests) sting-expanded) t))

(defconst sting-result-indicator "⬤")

(defun sting-buffer ()
  (get-buffer-create "*sting*"))

(display-buffer-in-side-window (sting-buffer)
                               '((side . right)
                                 (window-width . 0.2)))

(defface sting-no-result-indicator-face '((t :foreground "white")) "")
(defface sting-success-indicator-face '((t :foreground "green")) "")
(defface sting-failure-indicator-face '((t :foreground "red")) "")
(defface sting-timeout-indicator-face '((t :foreground "orange")) "")

(with-current-buffer (sting-buffer)
  (read-only-mode -1))


(defun insert-newline ()
  (insert "
"))

(defun insert-test (test)
  (let ((report (gethash test sting-reports)))
    (insert (propertize sting-result-indicator
                        'face (etypecase report
                                (null 'sting-no-result-indicator-face)
                                (sting-pass-report 'sting-success-indicator-face)
                                (sting-fail-report (ecase (sting-fail-report-kind report)
                                                     (:assertion 'sting-failure-indicator-face)
                                                     (:timeout 'sting-timeout-indicator-face))))
                        'sting-test test))
    (insert (propertize " " 'sting-test test))
    (let* ((package (symbol-name (sting-test-package test)))
           (name (symbol-name (sting-test-name test)))
           (button-size (+ (length package)
                           (length name)
                           3))) ; grab the indicator, the space and the /
      (insert (propertize package 'face 'font-lock-builtin-face 'sting-test test))
      (insert (propertize "/" 'sting-test test))
      (insert (propertize name 'face 'font-lock-function-name-face 'sting-test test))
      (let ((button (make-button (- (point) button-size) (point)
                                 'face nil
                                 'follow-link t
                                 'action (lambda (button)
                                           (let ((test (button-get button 'test)))
                                             (setf (gethash test sting-expanded)
                                                   (not (gethash test sting-expanded)))
                                             (repaint-buffer))))))
        ;; put test reference inside the button because Elisp closures...
        (button-put button 'test test)))
    (when (gethash test sting-expanded)
      (let ((description (sting-test-description test)))
        (when (and description
                   (not (string= description "")))
          (insert-newline)
          (insert (propertize (format "   %s" description)
                              'face 'font-lock-comment-face))))
      (when (and (sting-fail-report-p report)
                 (eql (sting-fail-report-kind report) :timeout))
        (insert-newline)
        (insert "   Timeout after ")
        (insert (propertize (format "%d" (sting-fail-report-timeout-seconds report))
                            'face 'sting-timeout-indicator-face))
        (insert "s")))
    (insert-newline)))

(defun repaint-buffer ()
  (with-current-buffer (sting-buffer)
    (let ((point (point)))
      (erase-buffer)
      (dolist (test sting-loaded-tests)
        (insert-test test))
      (goto-char point))))

(repaint-buffer)

(defun sting-run ()
  (interactive)
  ;; Rewind one character back until a header is found (it has the needed property sting-test).
  ;; That way, we can C-c C-c inside the description of an expanded test and still having it run again.
  (let ((test (loop for p = (point) then (1- p)
                    for test = (get-text-property p 'sting-test)
                    while (and (> p 0)
                               (not test))
                    finally (return test))))
    (if test
        (message "executing %s" test)
      (message "no test found at point"))))

(with-current-buffer (sting-buffer)
  (local-set-key (kbd "C-c C-c") 'sting-run))

;; At the very end
(with-current-buffer (sting-buffer)
  ;; (evil-emacs-state 1)
  (read-only-mode))
