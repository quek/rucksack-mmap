(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :rucksack)
  (require :bordeaux-threads))

(defpackage :rucksack-que-test
  (:use :cl :rucksack))

(in-package :rucksack-que-test)

(defparameter *db* "/tmp/rucksack-que-test/")

(unless *rucksack*
  (setf *rucksack* (open-rucksack *db*)))

(with-transaction ()
  (defclass foo ()
    ((name :accessor name :initarg :name :initform :unknow
           :index :string-index)
     (value :accessor value :initarg :value :initform 0))
    (:index t)
    (:metaclass persistent-class)))

(defmethod print-object ((object foo) stream)
  (print-unreadable-object (object stream)
    (format stream "~a ~a ~d" (object-id object)
            (name object) (value object))))

(defun delete-all-foo ()
  "(delete-all-foo)"
  (with-transaction ()
    (let (r)
      (rucksack-do-class (i 'foo)
        (push i r))
      (mapc (lambda (x)
              (rucksack-delete-object *rucksack* x))
            r))))

(defun run ()
  (delete-all-foo)
  (with-transaction ()
    (loop for i from 1 to 1000
          do (make-instance 'foo :name (format nil "foo~d" i))))
  (let ((threads
         (loop repeat 2
               collect (bt:make-thread
                        (lambda ()
                          (loop repeat 1 do
                            (with-transaction (:read-only t)
                              (rucksack-do-class (i 'foo)
                                (print (name i))))))))))
    (mapcar #'bt:join-thread threads)))
;;(time (run))