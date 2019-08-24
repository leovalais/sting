(in-package :sting)

(define-condition assertion-error (error)
  ((description :initarg :description
                :initform ""
                :reader description)
   (assertion :initarg :assertion
              :initform 'assert
              :reader assertion)
   (form :initarg :form
         :reader form)))

(defclass valued-form ()
  ((value :initarg :value
          :reader value)
   (form :initarg :form
         :reader form)))

(defun fail (assertion-error)
  (restart-case (error assertion-error)
    (ignore ()
      :report "Ignores the failed assertion and evaluates it to NIL"
      nil)
    #+sbcl (use-value (x)
      :report "Ignores the failed assertion and evaluates it to the given value"
      :interactive sb-kernel::read-evaluated-form
      x)))



(define-condition boolean-assertion-error (assertion-error)
  ((actual :initarg :actual
           :type valued-form
           :reader actual)
   (expected :initarg :expected
             :type boolean
             :reader expected)))

(defmacro assert (test-form &key (desc ""))
  (with-gensyms (x)
    `(let ((,x ,test-form))
       (if ,x
           t
           (fail (make-condition 'boolean-assertion-error
                                 :assertion 'assert
                                 :form '(assert ,test-form)
                                 :description ,desc
                                 :actual (make-instance 'valued-form
                                                        :value ,x
                                                        :form ',test-form)
                                 :expected t))))))

(defmacro assert-not (test-form &key (desc ""))
  (with-gensyms (x)
    `(let ((,x ,test-form))
       (if ,x
           (fail (make-condition 'boolean-assertion-error
                                 :assertion 'assert-not
                                 :form '(assert-not ,test-form)
                                 :description ,desc
                                 :actual (make-instance 'valued-form
                                                        :value ,x
                                                        :form ',test-form)
                                 :expected nil))
           t))))



(define-condition no-error-assertion-error (assertion-error)
  ((actual :initarg :actual
           :type valued-form
           :reader actual)
   (expected :initarg :expected
             :type symbol)))

(define-condition wrong-error-assertion-error (assertion-error)
  ((signalled :initarg :signalled
              :type valued-form
              :reader signalled)
   (expected :initarg :expected
             :type symbol)))

(defmacro assert-error (error-name test-form &key (desc ""))
  (with-gensyms (err x)
    `(handler-case (progn
                     (let ((,x ,test-form))
                       (fail (make-condition 'no-error-assertion-error
                                             :assertion 'assert-error
                                             :form '(assert-error ,test-form)
                                             :description ,desc
                                             :actual (make-instance 'valued-form
                                                                    :value ,x
                                                                    :form ',test-form)
                                             :expected ',error-name))
                       ,x))
       (,error-name (,err)
         (declare (ignore ,err))
         t)
       (condition (,err)
         (fail (make-condition 'wrong-error-assertion-error
                               :assertion 'assert-error
                               :description ,desc
                               :signalled (make-instance 'valued-form
                                                         :value ,err
                                                         :form ',test-form)
                               :expected ',error-name))))))



(define-condition equality-assertion-error (assertion-error)
  ((expected :initarg :expected
             :type valued-form
             :reader expected)
   (actual :initarg :actual
           :type valued-form
           :reader actual)))

(define-condition inequality-assertion-error (assertion-error)
  ((value :initarg :value
          :type valued-form
          :reader value)))

(defmacro assert-= (expected actual &key (desc ""))
  (with-gensyms (e a)
    `(let ((,e ,expected)
           (,a ,actual))
       (if (= ,e ,a)
           t
           (fail (make-condition 'equality-assertion-error
                                 :assertion 'assert-=
                                 :form '(assert-= ,expected ,actual)
                                 :description ,desc
                                 :expected (make-instance 'valued-form :value ,e :form ',expected)
                                 :actual (make-instance 'valued-form :value ,a :form ',actual)))))))

(defmacro assert-/= (value actual &key (desc ""))
  (with-gensyms (v a)
    `(let ((,v ,value)
           (,a ,actual))
       (if (/= ,v ,a)
           t
           (fail (make-condition 'inequality-assertion-error
                                 :assertion 'assert-/=
                                 :form '(assert-/= ,value ,actual)
                                 :description ,desc
                                 :value (make-instance 'valued-form
                                                       :form ',value
                                                       :value ,v)))))))



(define-condition cmp-assertion-error (assertion-error)
  ((operand-1 :initarg :operand-1
              :type valued-form
              :reader operand-1)
   (operand-2 :initarg :operand-2
              :type valued-form
              :reader operand-2)))

(defmacro assert-< (op-1 op-2 &key (desc ""))
  "(`assert-<' a b) asserts whether a < b. Signals a `cmp-assertion-error'."
  (with-gensyms (a b)
    `(let ((,a ,op-1) (,b ,op-2))
       (if (< ,a ,b)
           t
           (fail (make-condition 'cmp-assertion-error
                                 :assertion 'assert-<
                                 :form '(assert-< ,op-1 ,op-2)
                                 :description ,desc
                                 :operand-1 (make-instance 'valued-form :value ,a :form ',op-1)
                                 :operand-2 (make-instance 'valued-form :value ,b :form ',op-2)))))))

(defmacro assert-> (op-1 op-2 &key (desc ""))
  "(`assert->' a b) asserts whether a > b. Signals a `cmp-assertion-error'."
  (with-gensyms (a b)
    `(let ((,a ,op-1) (,b ,op-2))
       (if (> ,a ,b)
           t
           (fail (make-condition 'cmp-assertion-error
                                 :assertion 'assert->
                                 :form '(assert-> ,op-1 ,op-2)
                                 :description ,desc
                                 :operand-1 (make-instance 'valued-form :value ,a :form ',op-1)
                                 :operand-2 (make-instance 'valued-form :value ,b :form ',op-2)))))))

(defmacro assert-<= (op-1 op-2 &key (desc ""))
  "(`assert-<=' a b) asserts whether a ≤ b. Signals a `cmp-assertion-error'."
  (with-gensyms (a b)
    `(let ((,a ,op-1) (,b ,op-2))
       (if (<= ,a ,b)
           t
           (fail (make-condition 'cmp-assertion-error
                                 :assertion 'assert-<=
                                 :form '(assert-<= ,op-1 ,op-2)
                                 :description ,desc
                                 :operand-1 (make-instance 'valued-form :value ,a :form ',op-1)
                                 :operand-2 (make-instance 'valued-form :value ,b :form ',op-2)))))))

(defmacro assert->= (op-1 op-2 &key (desc ""))
  "(`assert->=' a b) asserts whether a ≥ b. Signals a `cmp-assertion-error'."
  (with-gensyms (a b)
    `(let ((,a ,op-1) (,b ,op-2))
       (if (>= ,a ,b)
           t
           (fail (make-condition 'cmp-assertion-error
                                 :assertion 'assert->=
                                 :form '(assert->= ,op-1 ,op-2)
                                 :description ,desc
                                 :operand-1 (make-instance 'valued-form :value ,a :form ',op-1)
                                 :operand-2 (make-instance 'valued-form :value ,b :form ',op-2)))))))
