(in-package :sting)

(define-condition assertion-error (error)
  ((description :initarg :description
                :initform ""
                :reader description)
   (form :initarg :form
         :reader form)
   (data :initarg :data
         :reader data)))

(define-condition no-error-signalled (assertion-error)
  ((expected-error :initarg :expected-error
                   :reader expected-error)))

(defun fail (value test-form description)
  (restart-case (error 'assertion-error :description description
                                        :form test-form
                                        :data value)
    (ignore ()
      :report "Ignores the failed assertion and evaluates it to NIL"
      nil)
    (use-value (x)
      :report "Ignores the failed assertion and evaluates it to the given value"
      :interactive sb-kernel::read-evaluated-form
      x)))

(defmacro assert (test-form &key (desc ""))
  (with-gensyms (x)
    `(let ((,x ,test-form))
       (if ,x
           ,x
           (fail ,x ',test-form ,desc)))))


(defmacro assert-not (test-form &key (desc ""))
  (with-gensyms (x)
    `(let ((,x ,test-form))
       (if ,x
           (fail ,x ',test-form ,desc)))))

(defmacro assert-error (error-name test-form &key (desc ""))
  (with-gensyms (err x)
    `(handler-case (progn
                     (let ((,x ,test-form))
                       (fail :no-error ',test-form ,desc)
                       ,x))
       (,error-name (,err)
         (declare (ignore ,err)))
       (condition (,err)
         (fail ,err ',test-form ,desc)))))

;; (assert-error division-by-zero (/ 1 0))

(defmacro define-assertion (name function &rest arguments)
  `(defmacro ,name (,@arguments &key (desc ""))
     ;; FIXME find a better way of writing it (maybe using nested backquotes)
     (list 'assert (list ',function ,@arguments) :desc desc)))

(define-assertion assert-= = a b)
(define-assertion assert-/= /= a b)
(define-assertion assert-< < a b)
(define-assertion assert-> > a b)
(define-assertion assert-<= <= a b)
(define-assertion assert->= >= a b)
