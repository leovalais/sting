;;; generic-rpc.el --- A generic RPC API supporting both SLIME and SLY. -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq sly-enable-evaluate-in-emacs t)

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar sting-lisp-backend :slynk
    "Tells Sting which backend it should try to use. Possible values are:
- :swank (for SLIME)
- :slynk (defalut, for SLY)")

  (defmacro define-rpc (name lambda-list &rest body)
    "Defines the RPC function `NAME'."
    (ecase sting-lisp-backend
      (:swank `(defslimefun ,name ,lambda-list ,@body))
      (:slynk `(defun ,name ,lambda-list ,@body)))))

(defun funcall-rpc (name &rest args)
  "Calls the RPC function `NAME' with the given `ARGS'.
Waits for `NAME' to return and returns its values."
  (ecase sting-lisp-backend
    (:swank (slime-eval (cons name args)))
    (:slynk (sly-eval (cons name args)))))

(defun funcall-rpc-no-wait (name &rest args)
  "Calls the RPC function `NAME' with the given `ARGS'.
Does ot wait for `NAME' to return."
  (ecase sting-lisp-backend
    (:swank (slime-eval-async (cons name args)))
    (:slynk (sly-eval-async (cons name args)))))

;;; generic-rpc.el ends here
