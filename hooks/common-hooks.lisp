(in-package :sting)

(defun compiled-in-emacs? (test)
  (the boolean
       (not (not (getf (source-info test) :buffer)))))

(defmethod add-test-in :after ((tc test-container) (test test))
  (ensure-emacs-connected)
  (when (and *auto-run*
             (compiled-in-emacs? test))
    (send-tests :tests (list test)
                :wait? t)
    (emacs-run (list test))))

;;; NOTE: `find-source-info' is not called yet so it does not matter that it is
;;; compiled after this function.
(define-define-test-hook :before-test-defun (test-gensym &key &allow-other-keys)
  `(setf (source-info ,test-gensym)
         (find-source-info)))
