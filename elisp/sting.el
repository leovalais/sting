;;; sting.el --- Emacs client for the Common Lisp testing library sting  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Valais Léo

;; Author: Léo Valais <leo.valais97@gmail.com>
;; Keywords: lisp, convenience, tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(load "generic-rpc")
(load "sting-core")
(load "commands")

(defvar sting-loaded-indicator "●")
(defvar sting-success-indicator "✓")
(defvar sting-failure-indicator "✗")
(defvar sting-timeout-indicator "∞")
(defvar sting-running-indicator "⊙")
(defconst sting-buffer-name "*sting*")
(defconst sting-buffer-name-regexp "^\\*sting\\*$")

(defvar sting-show-snippets nil)

(defvar sting-auto-bring-buffer-policy :auto
  "Determines when to bring the sting buffer automatically.
Possible values are:
- :always
- :auto (default)
- :never
The buffer can always be toggled using `sting-toggle-window'.")

(defvar sting-mode-map (make-sparse-keymap))
(define-key sting-mode-map (kbd "C-c C-c") 'sting-run)
(define-key sting-mode-map (kbd "C-c C-g") 'sting-open-source-interactive)

(define-derived-mode sting-mode fundamental-mode "sting"
  "blah"
  (use-local-map sting-mode-map)
  (sting-connect))

(add-to-list 'auto-mode-alist `(,sting-buffer-name-regexp . sting-mode))

(global-set-key (kbd "C-c t w") 'sting-toggle-window)
(global-set-key (kbd "C-c t TAB") 'sting-toggle-window)
(global-set-key (kbd "C-c t p") 'sting-run-package-interactive)
(global-set-key (kbd "C-c t a") 'sting-run-all)
(global-set-key (kbd "C-c t f") 'sting-run-failed)
(global-set-key (kbd "C-c t l l") 'sting-load-all-tests)
(global-set-key (kbd "C-c t l a") 'sting-load-all-tests)
(global-set-key (kbd "C-c t l p") 'sting-load-package-interactive)
(global-set-key (kbd "C-c t c") 'sting-clear)
(global-set-key (kbd "C-c t s") 'sting-sort)


(defun sting-ensure-state-bring-buffer (plist)
  (ecase (or (getf plist :bring-buffer?)
             :if-possible)
    ((:yes :always t)
     (unless (and (not (eql sting-auto-bring-buffer-policy :never))
                  (get-buffer-window (sting-buffer)))
       (sting-toggle-window)))
    ((:if-possible :auto)
     (when (and (eql sting-auto-bring-buffer-policy :always)
                (not (get-buffer-window (sting-buffer))))
       (sting-toggle-window)))
    ((:no :never nil))))

(add-hook 'sting-ensure-state-hook #'sting-ensure-state-bring-buffer)


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

(defface sting-loaded-face '((t :foreground "gray")) "")
(defface sting-success-face '((t :foreground "#30d158")) "")
(defface sting-failure-face '((t :foreground "#ff453a")) "")
(defface sting-timeout-face '((t :foreground "#ff9f0a")) "")
(defface sting-running-face '((t :foreground "#5e5ce6")) "")
(defface sting-property-face '((t :underline t)) "")
(defface sting-lisp-face '((t :foreground "#ffd60a")) "")

(defun insert-newline ()
  (insert "
"))

(defun sting-insert-indentation ()
  (insert "  "))

(defun sting-insert-property (prop &rest values)
  (sting-insert-indentation)
  (insert (propertize (format "%s:" prop) 'face 'sting-property-face))
  (insert " ")
  (let* ((len (+ (length prop) 5))
         (pad (make-string len ? )))
    (dolist (val values)
      (insert (replace-regexp-in-string "\n" (format "\n%s" pad) val)))))

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

(defun sting-insert-assertion-error-data-cell (dc)
  (insert-newline)
  (destructuring-bind (prop . value) dc
    (let ((prop (format "%s" prop)))
      (etypecase value
        (string
         (sting-insert-property prop value))
        (sting-valued-form
         (sting-insert-property (format "%s form" prop)
                                (propertize (format "%s" (sting-valued-form-form value))
                                            'face 'sting-lisp-face))
         (insert-newline)
         (sting-insert-property (format "%s value" prop)
                                (format "%s" (sting-valued-form-value value))))))))

(defun sting-insert-assertion-error (err)
  (let ((form (sting-error-form err))
        (description (sting-error-description err))
        (data (sting-error-data err)))
    (sting-insert-property "Form" (propertize form 'face 'sting-lisp-face))
    (when (and description
               (not (string= description "")))
      (insert-newline)
      (sting-insert-property "Failure desc."
                             (propertize description
                                         'face 'font-lock-comment-face)))
    (mapc #'sting-insert-assertion-error-data-cell data)))

(defun sting-insert-test-expansion (test)
  (let ((description (sting-test-description test))
        (source-info (sting-test-source-info test))
        (report (sting-get-report test)))
    (when (and description
               (not (string= description "")))
      (insert-newline)
      (sting-insert-indentation)
      (insert (propertize description 'face 'font-lock-comment-face)))
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
                                             'face 'sting-timeout-face)
                                 "s"))
         (:assertion
          (insert-newline)
          (sting-insert-assertion-error  (sting-fail-report-error report))))))
    (when (and source-info sting-show-snippets)
      (insert-newline)
      (insert "```") (insert-newline)
      (insert (getf source-info :snippet))
      (insert "'''") (insert-newline))))

(defun insert-test (test)
  (let ((report (sting-get-report test)))
    (insert (etypecase report
              (null (propertize sting-loaded-indicator 'face 'sting-loaded-face))
              ((eql :running) (propertize sting-running-indicator 'face 'sting-running-face))
              (sting-pass-report (propertize sting-success-indicator 'face 'sting-success-face))
              (sting-fail-report (ecase (sting-fail-report-kind report)
                                   (:assertion (propertize sting-failure-indicator 'face 'sting-failure-face))
                                   (:timeout (propertize sting-timeout-indicator 'face 'sting-timeout-face))))))
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
                                             (run-hooks 'sting-update-data-hook))))))
        ;; put test reference inside the button because Elisp closures...
        (button-put button 'test test)))
    (when (gethash test sting-expanded)
      (sting-insert-test-expansion test)
      (insert-newline))
    (insert-newline)))

(defun sting-insert-stats ()
  (let* ((total (length sting-loaded-tests))
         (reports (hash-table-values sting-reports)))
    (if reports
        (let* ((passed (count-if #'sting-pass-report-p reports))
               (assertions (count-if #'sting-failed-report-assertion-p reports))
               (timeouts (count-if #'sting-failed-report-timeout-p reports))
               (ratio (/ (float passed) (float total))))
          (flet ((colored (n face)
                          (propertize (format "%d" n)
                                      'face face)))
            (setq mode-line-misc-info
                  (format "%s·%s·%s/%d (%3.1f%%)"
                          (colored passed 'sting-success-face)
                          (colored assertions 'sting-failure-face)
                          (colored timeouts 'sting-timeout-face)
                          total
                          (* 100 ratio)))))
      (setq mode-line-misc-info "∅"))))

(defun repaint-buffer ()
  (with-current-buffer (sting-buffer)
    (let ((point (point)))
      (erase-buffer)
      (dolist (test sting-loaded-tests)
        (insert-test test))
      (goto-char point))
    (sting-insert-stats)))

(add-hook 'sting-update-data-hook #'repaint-buffer)


(provide 'sting)
;;; sting.el ends here
