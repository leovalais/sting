(defconst sting-result-indicator "⬤")
(defconst sting-buffer-name "*sting*")
(defconst sting-buffer-name-regexp "^\\*sting\\*$")

(defvar sting-show-snippets nil)
(defvar sting-colorize-snippets nil
  "WARNING: Highly experimental and very likely to break everything!")


(defvar sting-mode-map (make-sparse-keymap))
(define-key sting-mode-map (kbd "C-c C-c") 'sting-run)
(define-key sting-mode-map (kbd "C-c C-g") 'sting-open-source-interactive)

(define-derived-mode sting-mode fundamental-mode "sting"
  "blah"
  ;; Useless: Emacs finds it automatically (<mode-name>-map naming).
  ;; (use-local-map my-mode-map)

  (when sting-colorize-snippets
    (mmm-add-classes '((sting-lisp
                      :submode lisp-mode
                      :front "```"
                      :back "'''")))
    (mmm-mode)))

(add-to-list 'auto-mode-alist `(,sting-buffer-name-regexp . sting-mode))

(global-set-key (kbd "C-c t l") 'sting-load-tests)
(global-set-key (kbd "C-c t w") 'sting-toggle-window)
(global-set-key (kbd "C-c t TAB") 'sting-toggle-window)


(defun sting-buffer ()
  (get-buffer-create sting-buffer-name))

(defun sting-toggle-window ()
  (interactive)
  (let ((window (get-buffer-window (sting-buffer))))
    (if window
        (delete-window window)
      (display-buffer-in-side-window (sting-buffer)
                                     '((side . right)
                                       (window-width . 0.25))))))

(defface sting-no-result-indicator-face '((t :foreground "white")) "")
(defface sting-success-indicator-face '((t :foreground "#30d158")) "")
(defface sting-failure-indicator-face '((t :foreground "#ff453a")) "")
(defface sting-timeout-indicator-face '((t :foreground "#ff9f0a")) "")
(defface sting-running-indicator-face '((t :foreground "#5e5ce6")) "")
(defface sting-property-face '((t :underline t)) "")

(defun insert-newline ()
  (insert "
"))

(defun sting-insert-indentation ()
  (insert "   "))

(defun sting-insert-property (prop &rest values)
  (sting-insert-indentation)
  (insert (propertize (format "%s:" prop) 'face 'sting-property-face))
  (insert " ")
  (dolist (val values)
    (insert val)))

(defun sting-make-simple-button (fun properties &rest text-to-insert)
  (mapc #'insert text-to-insert)
  (let* ((size (reduce #'+ text-to-insert :key #'length :initial-value 0))
         (p (point))
         (button (apply #'make-button
                        (- p size) p
                        'face nil
                        'follow-link t
                        'action fun
                        properties)))
    button))

(defun sting-insert-test-expansion (test)
  (let ((description (sting-test-description test))
        (source-info (sting-test-source-info test))
        (report (sting-get-report test)))
    (when (and description
               (not (string= description "")))
      (insert-newline)
      (sting-insert-indentation)
      (insert (propertize (format "%s" description)
                          'face 'font-lock-comment-face)))
    (when source-info
      (insert-newline)
      (let ((location (or (getf source-info :buffer)
                          (getf source-info :file)))
            (offset (getf source-info :offset)))
        (sting-insert-indentation)
        (sting-make-simple-button (lambda (btn)
                                    (sting-open-source (button-get btn 'sting-test)))
                                  (list 'sting-test test)
                                  (propertize location 'face 'compilation-info)
                                  ":"
                                  (propertize (format "%d" offset)
                                              'face 'compilation-line-number))))
    (etypecase report
      (null)
      ((eql :running))
      (sting-pass-report
       (insert-newline)
       (sting-insert-property "Eval values" (sting-pass-report-values report)))
      (sting-fail-report
       (ecase (sting-fail-report-kind report)
         (:timeout
          (insert-newline)
          (sting-insert-property "Timeout"
                                 (propertize (format "%d" (sting-fail-report-timeout-seconds report))
                                             'face 'sting-timeout-indicator-face)
                                 "s"))
         (:assertion
          (insert-newline)
          (sting-insert-property "Error" (sting-fail-report-error report))))))
    (when (and source-info sting-show-snippets)
      (insert-newline)
      (insert "```") (insert-newline)
      (insert (getf source-info :snippet))
      (insert "'''") (insert-newline))))

(defun insert-test (test)
  (let ((report (sting-get-report test)))
    (insert (propertize sting-result-indicator
                        'face (etypecase report
                                (null 'sting-no-result-indicator-face)
                                ((eql :running) 'sting-running-indicator-face)
                                (sting-pass-report 'sting-success-indicator-face)
                                (sting-fail-report (ecase (sting-fail-report-kind report)
                                                     (:assertion 'sting-failure-indicator-face)
                                                     (:timeout 'sting-timeout-indicator-face))))
                        'sting-test test))
    (insert (propertize " " 'sting-test test))
    (let* ((package (sting-test-package test))
           (name (sting-test-name test))
           (button-size (+ (length package)
                           (length name)
                           3))) ; grab the indicator, the space and the :
      (insert (propertize package 'face 'font-lock-builtin-face 'sting-test test))
      (insert (propertize ":" 'sting-test test))
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
      (sting-insert-test-expansion test)
      (insert-newline))
    (insert-newline)))

(defun repaint-buffer ()
  (with-current-buffer (sting-buffer)
    (let ((point (point)))
      (erase-buffer)
      (dolist (test sting-loaded-tests)
        (insert-test test))
      (goto-char point)
      (when sting-colorize-snippets
        (mmm-parse-buffer)))))


;; ;; At the very end
;; (with-current-buffer (sting-buffer)
;;   ;; (evil-emacs-state 1)
;;   (read-only-mode))
