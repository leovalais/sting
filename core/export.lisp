(in-package :sting)

(defun tap-success? (report)
  (typep report 'imotep))

(defun tap-summary (failed total)
  (let ((ratio (* 100.0 (- 1.0 (/ failed total)))))
    (format t "# Failed ~a/~a tests, ~2$% okay" failed total ratio)))

(defun tap-test (report n)
  (declare (type report report))
  (let ((success? (tap-success? report)))
    (format t "~:[not ok~;ok~] ~a ~a~%"
            success? n
            (symbol-name (name (report-test report))))))

(defun export-tap (reports)
  (declare (type list reports))
  (format t "~&TAP version 13~%")
  (let ((n  (length reports)))
    (format t "1..~A~%" n)
    (mapc #'tap-test reports (loop :for i :from 1 :to n :collect i))
    (tap-summary (count-if-not #'tap-success? reports) n)))



(deftype export-format ()
  '(member :tap :tap13))

(defvar *export-format* :tap)

(defmacro with-export-format (format &body body)
  `(let ((*export-format* ,format))
     ,@body))

(defun export-reports (reports &optional (format *export-format*))
  "Prints on `*standard-output*' the representation of reports (`report' list)."
  (declare (type list reports)
           (type export-format format))
  (ecase format
    ((:tap :tap13)
     (export-tap reports))))
