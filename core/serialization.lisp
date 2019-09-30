(in-package :sting)

(defun ppstr (sexp)
  (subseq (with-output-to-string (out)
            (pprint sexp out))
          1))

(defgeneric serialize (object))

(defgeneric serialize-assertion-error (err)
  (:method-combination append :most-specific-last))



(defmethod serialize ((test test))
  (with-slots (name description source-info) test
    (list :tag :test
          :name (symbol-name name)
          :package (package-name (symbol-package name))
          :description description
          :source-info source-info)))

(defmethod serialize ((report imotep))
  (with-slots (test values) report
    (list :tag :pass-report
          :test (serialize test)
          :values (format nil "~S" values))))

(defmethod serialize ((report failure))
  (with-slots (test kind failure-error timeout-seconds) report
    (list :tag :fail-report
          :test (serialize test)
          :kind kind
          :error (serialize failure-error)
          :timeout-seconds timeout-seconds)))

(defmethod serialize ((- trivial-timeout:timeout-error))
  (the string "TIMEOUT-ERROR"))

(defmethod serialize ((err assertion-error))
  (serialize-assertion-error err))

(defmethod serialize ((vf valued-form))
  (list :tag :valued-form
        :value (format nil "~S" (value vf))
        :form (ppstr (form vf))))



(defmethod serialize-assertion-error append ((err assertion-error))
  (list :tag :error
        :class (symbol-name (class-name (class-of err)))
        :assertion (ppstr (assertion err))
        :form (ppstr (form err))
        :description (description err)))

(defmethod serialize-assertion-error append ((err boolean-assertion-error))
  (list :actual (serialize (actual err))
        :expected (ppstr (expected err))))

(defmethod serialize-assertion-error append ((err no-error-assertion-error))
  (list :actual (serialize (actual err))
        :expected (ppstr (expected err))))

(defmethod serialize-assertion-error append ((err no-error-assertion-error))
  (list :signalled (serialize (signalled err))
        :expected (ppstr (expected err))))

(defmethod serialize-assertion-error append ((err equality-assertion-error))
  (list :actual (serialize (actual err))
        :expected (serialize (expected err))))

(defmethod serialize-assertion-error append ((err inequality-assertion-error))
  (list :value (serialize (value err))))

(defmethod serialize-assertion-error append ((err cmp-assertion-error))
  (list :operand-1 (serialize (operand-1 err))
        :operand-2 (serialize (operand-2 err))))
